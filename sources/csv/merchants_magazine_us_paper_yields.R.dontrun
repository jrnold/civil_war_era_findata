source("R/init.R")

### depends: data/merchants_magazine_us_paper.csv data/bond_metadata.json
merchants_file <- "data/merchants_magazine_us_paper.csv"
bond_metadata_file <- "data/bond_metadata.json"
outfile <- commandArgs(TRUE)[1]

#' Load prerequisite data
BOND_SERIES <- c("5's, 1874",
                 sprintf("6's, 1881 %s", c("Reg.", "Coup.")),
                 sprintf("5-20's, %s", c("Reg.", "Coup.")),
                 "10-40's", "7 3-10, 3 years")

gold_rates_actual <- read_csv("data/greenbacks_fill.csv") %>%
    mutate(gold_rate = 100 / mean,
           date = as.Date(date)) %>%
    select(date, gold_rate)

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
        data_frame(bond =
                   sprintf("us_6pct_1881_%s", c("jan", "jul")),
                   wgt = c(0.089, 0.911))
    }

#' For the sixes of 1881 apply to Jan and July Sixes of 1881
MATCH_BONDS[["6's, 1881 Reg."]] <-
    function(date) {
        data_frame(bond =
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
                           metadata = bond_metadata[[.$bond]],
                           gold_rates_actual = gold_rates_actual)
    ret[["date"]] <- .$date
    ret[["bond"]] <- .$bond
    ret[["series"]] <- .$series
    ret[["wgt"]] <- .$wgt
    select(ret, bond, date, series, wgt, everything())
  })

rfyields_date <- function(x) {
  rate <- filter(x,
                 series %in% c("6's, 1881 Coup.")) %>%
    `[[`("ytm_currency") %>%
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
                   ytm_goldrf = yields$yield,
                   duration_goldrf = yields$duration,
                   convexity_goldrf = yields$convexity,
                   date = .i$date,
                   bond = .i$bond
        )
    } else {
      ret[[i]] <-
        data_frame(govt_rate = rate,
                   ytm_goldrf = .i$ytm_currency,
                   duration_goldrf = .i$duration_currency,
                   convexity_goldrf = .i$convexity_currency,
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
.data[touse, "current_yield_currency"] <- 7.3 / .data$price_clean[touse]
.data[touse, "current_yield_gold"] <- 7.3 / (.data$price_clean[touse] * .data$gold_rate[touse]
)

touse <- (.data[["bond"]] == "us_seven_thirties_1864_aug_option"
          & .data[["date"]] >= as.Date("1864-2-19")
          & .data[["date"]] < as.Date("1864-8-19"))
.data[touse, "current_yield_currency"] <- 6.65 / .data$price_clean[touse]
.data[touse, "current_yield_gold"] <- 6.65 / (.data$price_clean[touse] * .data$gold_rate[touse]
)

touse <- (.data[["bond"]] == "us_seven_thirties_1864_aug_option"
          & .data[["date"]] >= as.Date("1864-8-19"))
.data[touse, "current_yield_currency"] <- 6 / .data$price_clean[touse]
.data[touse, "current_yield_gold"] <- 6 / (.data$price_clean[touse] * .data$gold_rate[touse]
)


touse <- (.data[["bond"]] == "us_seven_thirties_1864_oct_option"
          & .data[["date"]] < as.Date("1864-10-1"))
.data[touse, "current_yield_currency"] <- 7.3 / .data$price_clean[touse]
.data[touse, "current_yield_gold"] <- 7.3 / (.data$price_clean[touse] * .data$gold_rate[touse]
)

touse <- (.data[["bond"]] == "us_seven_thirties_1864_oct_option"
          & .data[["date"]] < as.Date("1864-10-1")
          & .data[["date"]] >= as.Date("1864-4-1"))
.data[touse, "current_yield_currency"] <- 6.65 / .data$price_clean[touse]
.data[touse, "current_yield_gold"] <- 6.65 / (.data$price_clean[touse] * .data$gold_rate[touse]
)

touse <- (.data[["bond"]] == "us_seven_thirties_1864_oct_option"
          & .data[["date"]] >= as.Date("1864-10-1"))
.data[touse, "current_yield_currency"] <- 6 / .data$price_clean[touse]
.data[touse, "current_yield_gold"] <- 6 / (.data$price_clean[touse] * .data$gold_rate[touse]
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

yield_note <- function(price, date, payout, maturity_date) {
  m <- difftime_years(maturity_date, date)
  y <- - log(price / payout) / m
  list(yield = y,
       duration = m,
       convexity = m ^ 2,
       maturity = m)
}

make_yields_note <- function(maturity_date, interest, pays_gold,
                             date, gold_rate, price_gold,
                             adjust_gold, adjust_currency) {
  price <-
    price_gold + adjust_gold + adjust_currency / gold_rate
  price_currency <- price_gold * gold_rate
  payout <- (1 + interest) * 100
  if (pays_gold) {
    payout_gold <- payout # in gold
    payout_currency <- payout_gold * gold_rate  # in currency
  } else {
    payout_gold <-  payout / gold_rate
    payout_currency <- payout # in currency
  }
  # Yields in currency
  yields1 <- yield_note(price_currency, date, payout_currency,
                        maturity_date)
  # Yields in gold
  yields2 <- yield_note(price, date, payout_gold,
                        maturity_date)
  # Yields with redemption at 5%
  yields3 <-
    future_gold_rates(maturity_date, date, gold_rate, r = 0.04) %>%
    yield_note(price, date, payout_currency * .$gold_rate, maturity_date)
  # Yields with redemption at 4%
  yields4 <-
    future_gold_rates(maturity_date, date, gold_rate, r = 0.05) %>%
    yield_note(price, date, payout_currency * .$gold_rate, maturity_date)
  # Yields with redemption at 5%
  yields5 <-
    future_gold_rates(maturity_date, date, gold_rate, r = 0.06) %>%
    yield_note(price, date, payout_currency * .$gold_rate, maturity_date)
  yields6 <-
    gold_cashflows_redemp_t(cashflows, date, gold_rate,
                            redemp_date = as.Date("1879-1-1")) %>%
    yield_note(price_currency, date, .)

  yields_actual <-
    currency_cashflows_actual(cashflows, gold_rates_actual) %>%
    yield_note(price_currency, date, .)
  
  data_frame(price = price,
             price_clean = price,
             gold_rate = gold_rate,
             ytm_currency = yields1$yield,
             maturity = yields$maturity,
             duration_currency = yields1$duration,
             convexity_currency = yields1$convexity,
             ytm_gold = yields2$yield,
             duration_gold = yields2$duration,
             convexity_gold = yields2$convexity,
             ytm_gold4pct = yields3$yield,
             duration_gold4pct = yields3$duration,
             convexity_gold4pct = yields3$convexity,
             ytm_gold5pct = yields4$yield,
             duration_gold5pct = yields4$duration,
             convexity_gold5pct = yields4$convexity,
             ytm_gold6pct = yields5$yield,
             duration_gold6pct = yields5$duration,
             convexity_gold6pct = yields5$convexity,
             ytm_actual = yields_actual$yield,
             duration_actual = yields_actual$duration,
             convexity_actual = yields_actual$convexity)
}

oneyr_old <-
  filter(.data, series == "1 year certificate, Old") %>%
  rowwise() %>%
  do({
    maturity_date <- .$date + months(12)
    out <- make_yields_note(
      maturity_date,
      interest = 0.06,
      pays_gold = FALSE,
      .$date,
      .$gold_rate,
      .$price_gold,
      .$adjust_gold,
      .$adjust_currency,
      gold_rate_actual = gold_rate_actual)         
    out[["date"]] <- .$date
    out[["series"]] <- "1 year certificate, Old"
    out[["bond"]] <- "us_1yr_note_1862"
    out[["wgt"]] <- 1
    out
  })


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

oneyr_new <-
  filter(.data, series == "1 year certificate, New") %>%
    rowwise() %>%
    do({
      maturity_date <- .$date + months(12)
      out <- make_yields_note(
        maturity_date,
        interest = 0.05,
        pays_gold = FALSE,
        .$date,
        .$gold_rate,
        .$price_gold,
        .$adjust_gold,
        .$adjust_currency,
        gold_rate_actual = gold_rate_actual) 
      out[["date"]] <- .$date
      out[["series"]] <- "1 year certificate, New"
      out[["bond"]] <- "us_1yr_note_1863"
      out[["wgt"]] <- 1
      out
    })

.data <-
    bind_rows(.data, oneyr_old, oneyr_new) %>%
    mutate(registered = as.integer(grepl("_reg$", series)))

write_csv(.data, file = outfile)
