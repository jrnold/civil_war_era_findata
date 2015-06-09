source("R/.init.R")

sysargs <- commandArgs(TRUE)
merchants_file <- sysargs[1]
merchants_file <- "data/merchants_magazine_us_paper.csv"
bond_metadata_file <- sysargs[2]
bond_metadata_file <- "data/bond_metadata.json"
outfile <- sysargs[3]
outfile <- "data/merchants_magazine_us_paper_yields.csv"

#' Load prerequisite data
BOND_SERIES <- c("5's, 1874",
                 sprintf("6's, 1881 %s", c("Reg.", "Coup.")),
                 sprintf("5-20's, %s", c("Reg.", "Coup.")),
                 "10-40's", "7 3-10, 3 years")

merchants <-
    (mutate(read_csv(merchants_file),
            date = as.Date(date, "%Y-%m-%d"))
     %>% select(date, series, adjust_gold, adjust_currency,
                price_gold, gold_rate, is_clean)
     %>% filter(!is.na(price_gold)))

merchants_bonds <-
    filter(merchants, series %in% BOND_SERIES)

bond_metadata <- get_bond_metadata(bond_metadata_file)

#' # Series -> BONDS weights
MATCH_BONDS <- list()

MATCH_BONDS[["5's, 1874"]] <-
    function(date) make_bond_table("us_5pct_1874")

MATCH_BONDS[["6's, 1881 Coup."]] <-
    function(date) {
        data.frame(bond =
                   sprintf("us_6pct_1881_%s", c("jan", "jul")),
                   wgt = c(0.089, 0.911))
    }

#' For the sixes of 1881 apply to Jan and July Sixes of 1881
MATCH_BONDS[["6's, 1881 Reg."]] <-
    function(date) {
        data.frame(bond =
                   sprintf("us_6pct_1881_%s", c("jan", "jul")),
                   wgt = c(0.089, 0.911))
    }

#' The five-twenties are first quoted in Jan 1, 1865.
#' So assume that the five-twenties quoted are the five-twenties of 1864, not 1862 or 1865.
MATCH_BONDS[["5-20's, Coup."]] <-
    function(date) {
        make_bond_table_regex("^us_five_twenty_of_1864", bond_metadata)
    }

MATCH_BONDS[["5-20's, Reg."]] <-
    function(date) {
        make_bond_table_regex("^us_five_twenty_of_1864", bond_metadata)
    }


MATCH_BONDS[["10-40's"]] <-
    function(date) {
        make_bond_table_regex("^us_ten_forty_", bond_metadata)
    }


MATCH_BONDS[["7 3-10, 3 years"]] <-
    function(date) {
        rbind(make_bond_table_regex("^us_seven_thirties_1864_(aug|oct)_option", bond_metadata))
    }

unmatched <- setdiff(unique(merchants_bonds$series), names(MATCH_BONDS))
if (length(unmatched)) {
    stop(sprintf("No entries for: %s", paste(unmatched, collapse = ", ")))
}

match_series_to_bonds <- function(series, date, ...) {
    MATCH_BONDS[[as.character(series)]](date)
}

.data <-
  plyr::mdply(merchants_bonds, match_series_to_bonds) %>%
  #slice(1:5000) %>%
  rowwise() %>%
  do({
    ret <- make_yields_etc(date = .$date,
                           bond = .$bond,
                           gold_rate = .$gold_rate,
                           price_gold = .$price_gold,
                           adjust_gold = .$adjust_gold,
                           adjust_currency = .$adjust_currency,
                           is_clean = .$is_clean,
                           metadata = bond_metadata[[.$bond]])
    ret[["date"]] <- .$date
    ret[["bond"]] <- .$bond
    ret[["series"]] <- .$series
    ret[["wgt"]] <- .$wgt
    select(ret, bond, date, series, wgt, everything())
  })

rfyields_date <- function(x) {
  rate <- filter(x,
                 series %in% c("6's, 1881 Coup.")) %>%
    `[[`("ytm1") %>%
    mean()

  ret <- vector(length = nrow(x), mode = "list")
  for (i in seq_len(nrow(x))) {
    .i <- x[i, , drop = FALSE]
    if(.i$gold_rate != 1) {
      yields <-
        gold_cashflows_redemp(bond_metadata[[.i$bond]]$cashflows,
                              .i$date,
                              .i$gold_rate,
                              r = rate) %>%
        yield_to_maturity2(.i$price * .i$gold_rate, .i$date, .)
      ret[[i]] <-
        data_frame(govt_rate = rate,
                   ytm6 = as.numeric(yields),
                   duration6 = attr(yields, "duration"),
                   convexity6 = attr(yields, "convexity"),
                   maturity6 = attr(yields, "maturity"),
                   date = .i$date,
                   bond = .i$bond
        )
    } else {
      ret[[i]] <-
        data_frame(govt_rate = rate,
                   ytm6 = .i$ytm1,
                   duration6 = .i$duration1,
                   convexity6 = .i$convexity1,
                   maturity6 = .i$maturity1,
                   date = .i$date,
                   bond = .i$bond)
    }
  }
  bind_rows(ret)
}

#' Get currency rate for the government for each time period
yields6 <-
  .data %>%
  ungroup() %>%
  group_by(date) %>%
  do(rfyields_date(.))

.data %<>% left_join(yields6, by = c("date", "bond"))

#'
#' Add current yields for seven_thirties. This is handled separately because
#' the coupon payments change before and after they can be converted to the 6's of 1881.
#'
#' - 7.3 percent before the option exercised
#' - (7.3 + 6) / 2 percent in the period before the last 3.65 coupon
#' - 6 percent after the option exercised
#'
touse <- (.data[["bond"]] == "us_seven_thirties_1864_aug_option"
          & .data[["date"]] < as.Date("1864-8-19"))
.data[touse, "current_yield1"] <- 7.3 / .data$price_clean[touse]
.data[touse, "current_yield2"] <- 7.3 / (.data$price_clean[touse] * .data$gold_rate[touse]
)

touse <- (.data[["bond"]] == "us_seven_thirties_1864_aug_option"
          & .data[["date"]] >= as.Date("1864-2-19")
          & .data[["date"]] < as.Date("1864-8-19"))
.data[touse, "current_yield1"] <- 6.65 / .data$price_clean[touse]
.data[touse, "current_yield2"] <- 6.65 / (.data$price_clean[touse] * .data$gold_rate[touse]
)

touse <- (.data[["bond"]] == "us_seven_thirties_1864_aug_option"
          & .data[["date"]] >= as.Date("1864-8-19"))
.data[touse, "current_yield1"] <- 6 / .data$price_clean[touse]
.data[touse, "current_yield2"] <- 6 / (.data$price_clean[touse] * .data$gold_rate[touse]
)


touse <- (.data[["bond"]] == "us_seven_thirties_1864_oct_option"
          & .data[["date"]] < as.Date("1864-10-1"))
.data[touse, "current_yield1"] <- 7.3 / .data$price_clean[touse]
.data[touse, "current_yield2"] <- 7.3 / (.data$price_clean[touse] * .data$gold_rate[touse]
)

touse <- (.data[["bond"]] == "us_seven_thirties_1864_oct_option"
          & .data[["date"]] < as.Date("1864-10-1")
          & .data[["date"]] >= as.Date("1864-4-1"))
.data[touse, "current_yield1"] <- 6.65 / .data$price_clean[touse]
.data[touse, "current_yield2"] <- 6.65 / (.data$price_clean[touse] * .data$gold_rate[touse]
)

touse <- (.data[["bond"]] == "us_seven_thirties_1864_oct_option"
          & .data[["date"]] >= as.Date("1864-10-1"))
.data[touse, "current_yield1"] <- 6 / .data$price_clean[touse]
.data[touse, "current_yield2"] <- 6 / (.data$price_clean[touse] * .data$gold_rate[touse]
)

#'
#' One year old (Certificates of Indebtedness)
#'
#' Issued under the Act of March 1, 1862 (12 Stat 352).
#' Paid 6 percent interest on maturity (1 year after issue).
#' Act of March 3, 1863 (12 Stat 710) made the interest payable
#' in lawful money.
#'
#' Quoted from 1862-3-26 to 1864-3-26. For yield calculations,
#' I will assume that the interest is always payable in lawful money rather than currency.
#'
#' - Bayley, [p. 81](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA81), [p. 158-159](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA158)
#' - De Knight, [p. 86-87](http://books.google.com/books?id=0cQmAQAAMAAJ&pg=PA86)
#' - Noll ?
#' - Annual Report of the Treasury 1863, [p. 44-45](https://fraser.stlouisfed.org/docs/publications/treasar/AR_TREASURY_1863.pdf#page=52)
#'
#'
make_yields_note <- function(maturity_date, interest, pays_gold,
                             date,  gold_rate, price_gold,
                             adjust_gold, adjust_currency) {
  price <-
    price_gold + adjust_gold + adjust_currency / gold_rate
  price_currency <- price_gold * gold_rate
  future_rate <- future_gold_rates(maturity_date, date, gold_rate)$gold_rate
  payout_currency <- 100 * (1 + interest)
  if (pays_gold) {
    payout_gold <- payout_currency # in gold
    payout_currency2 <- payout_currency * future_rate  # in currency
  } else {
    payout_gold <- payout_currency / gold_rate # in gold
    payout_currency2 <- payout_currency # in currency
  }
  maturity_time <- difftime_years(maturity_date, date)
  ytm1 <- - log(price / payout_gold) / maturity_time
  ytm2 <- - log(price_currency / payout_currency2 ) / maturity_time
  ytm3 <- - log(price_currency / payout_currency ) / maturity_time
  duration1 <- duration2 <- duration3 <- maturity_time
  convexity1 <- convexity2 <- convexity3 <- maturity_time ^ 2
  maturity1 <- maturity2 <- maturity3 <- maturity_time
  data.frame(price = price,
             price_clean = price,
             gold_rate = gold_rate,
             ytm1 = ytm1,
             duration1 = duration1,
             convexity1 = convexity1,
             maturity1 = maturity1,
             ytm2 = ytm2,
             duration2 = duration2,
             convexity2 = convexity2,
             maturity2 = maturity2,
             ytm3 = ytm3,
             duration3 = duration3,
             convexity3 = convexity3,
             maturity3 = maturity3)
}

make_yield_1yr_old <- function(.data, m, wgt) {
    filter(.data, series == "1 year certificate, Old") %>%
    rowwise() %>%
    do({
      maturity_date <- .$date + months(m)
      out <- make_yields_note(
        maturity_date,
        interest = 0.06,
        pays_gold = TRUE,
        .$date,
        .$gold_rate,
        .$price_gold,
        .$adjust_gold,
        .$adjust_currency)
      out[["date"]] <- .$date
      out[["series"]] <- "1 year certificate, Old"
      out[["bond"]] <- sprintf("us_cert_indebt_1862_%dmon", m)
      out[["wgt"]] <- wgt
      out
    })
}

oneyr_old <-
  bind_rows(make_yield_1yr_old(merchants, 6, 0.5),
            make_yield_1yr_old(merchants, 12, 0.5))

#'
#' One Year New (Treasury Notes of 1863)
#'
#' Issued under the Act of March 3, 1863.
#' Paid interest of 5 percent on maturity (1 year after issue).
#' Both principal and interest paid in lawful currency.
#'
#'
#' These were quoted after 1863-4-25.
#'
#' - Bayley, [p. 82-84](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA82),  [p. 161](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA161)
#' - De Knight, [p. 88-89](http://books.google.com/books?id=0cQmAQAAMAAJ&pg=PA88)
#' - Noll, Vol 8, [p. 304](http://www.franklinnoll.com/Vol_8.pdf#page=305)
#' - Annual Report of the Treasury 1865, [p. 52-53](https://fraser.stlouisfed.org/docs/publications/treasar/AR_TREASURY_1865.pdf#page=56)

make_yield_1yr_new <- function(.data, m, wgt) {
  filter(.data, series == "1 year certificate, New") %>%
    rowwise() %>%
    do({
      maturity_date <- .$date + months(m)
      out <- make_yields_note(
        maturity_date,
        interest = 0.05,
        pays_gold = FALSE,
        .$date,
        .$gold_rate,
        .$price_gold,
        .$adjust_gold,
        .$adjust_currency)
      out[["date"]] <- .$date
      out[["series"]] <- "1 year certificate, New"
      out[["bond"]] <- sprintf("us_1yr_note_1863_%dmon", m)
      out[["wgt"]] <- wgt
      out
    })
}

oneyr_new <-
  bind_rows(make_yield_1yr_new(merchants, 6, 0.5),
            make_yield_1yr_new(merchants, 12, 0.5))


.data <-
    bind_rows(.data, oneyr_old, oneyr_new) %>%
    mutate(registered = as.integer(grepl("_reg$", series)))

write_csv(.data, file = outfile)
