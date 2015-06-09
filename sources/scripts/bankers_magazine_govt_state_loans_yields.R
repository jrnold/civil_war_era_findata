source("R/.init.R")

# TODO: remove coupon payments for states during ACW
# TODO: yields using assumed future redemption

sysargs <- commandArgs(TRUE)
bankers_file <- sysargs[1]
bankers_file <- "data/bankers_magazine_govt_state_loans.csv"
bond_metadata_file <- sysargs[2]
bond_metadata_file <- "data/bond_metadata.json"
outfile <- sysargs[3]
outfile <- "data/bankers_magazine_govt_state_loans_yields.csv"

#' Load prerequisite data
bankers <-
  (mutate(read_csv(bankers_file),
          date = as.Date(date, "%Y-%m-%d"))
   %>% select(date, series, is_clean, adjust_gold,
              adjust_currency, price_gold, gold_rate)
   %>% filter(!is.na(price_gold)))

bond_metadata <- get_bond_metadata(bond_metadata_file)

MATCH_BONDS <- list()

MATCH_BONDS[["California 7 per cents, 1870"]] <-
  function(date) make_bond_table("california_7pct_1870")

MATCH_BONDS[["California 7 per cents, 1877"]] <-
  function(date) make_bond_table("california_7pct_1877")

#'
#' The are many possible values of the Georgia issue. Denny & Co list 1868-1887.
#' However, the table in the Bankers' Magazine in 1857 lists 1872 as the only redemption date.
#' Assume that Bankers' continues to quote only the 1872 issue.
MATCH_BONDS[["Georgia 6 per cents"]] <-
  function(date) make_bond_table("georgia_6pct_1872")

#'
#' For Indiana 5 percent bonds, the redemption date is unkonwn.
#' So match it to virtual 5 percent bonds due over the entire range of bonds in the Bankers' data (1869-1892).
#' For weights to assign to each issue, use the empirical distribution of redemptions:
#'
#' - Kentucky: 1869-72
#' - Missouri: 1872
#' - North Carolina: 1873
#' - Pennsylvania: 1873
#' - Virginia: 1885-1890
#' - Tennessee: 1885-1892
#' - Georgia: 1872
#'
indiana_lookup <-
  make_bond_table_dist("indiana_5pct_%d",
                       list(kentucky = 1869:1872,
                            missouri = 1872,
                            north_carolina = 1873,
                            pennsylvania = 1871,
                            virginia = 1885:1890,
                            georgia = 1872))

MATCH_BONDS[["Indiana 5 per cents"]] <- function(date) indiana_lookup

#'
#' The bond metadata includes years 1868-1885, but the table of bonds in http://books.google.com/books?id=g2QmAQAAIAAJ&pg=PA332 only lists 1869-72.
MATCH_BONDS[["Kentucky 6 per cents"]] <-
  function(date) make_bond_table(sprintf("kentucky_6pct_%d", 1869:1872))

#' For Louisiana, no explicit range of redemption years can be found.
#' So assume that the probability of a given redemption year is the empirical distribuiton
#' of the known redemption years of those other state securities listed with it for all years
#' it was listed in Bankers' Magazine (with some smoothing).
#'
#'  - Kentucky: 1869-72
#'  - Missouri: 1872
#'  - North Carolina: 1873
#'  - Pennsylvania: 1873
#'  - Virginia: 1885-1890
#'  - Tennessee: 1885-1892

louisiana_lookup <-
  make_bond_table_dist("louisiana_6pct_%d",
                       list(kentucky = 1869:1872,
                            missouri = 1872,
                            north_carolina = 1873,
                            pennsylvania = 1871,
                            virginia = 1885:1890,
                            tennessee = 1885:1892))
MATCH_BONDS[["Louisiana 6 per cents"]] <- function(date) louisiana_lookup

#' Denny and Co list 1868-1890, but [Bankers' Magazine 1857](http://books.google.com/books?id=g2QmAQAAIAAJ&pg=PA332) lists only 1872.
MATCH_BONDS[["Missouri 6 per cents"]] <-
  function(date) make_bond_table("missouri_6pct_1872")

MATCH_BONDS[["North Carolina 6 per cents"]] <-
  function(date) make_bond_table("north_carolina_6pct_1873")

MATCH_BONDS[["Ohio 6 per cents, 1875"]] <-
  function(date) make_bond_table("ohio_6pct_1875")

MATCH_BONDS[["Ohio 6 per cents, 1886"]] <-
  function(date) make_bond_table("ohio_6pct_1886")

MATCH_BONDS[["Pennsylvania 5 per cents"]] <-
  function(date) make_bond_table("pennsylvania_5pct_1871")

#' US 6 per cents 1868 weights derived from value issued
MATCH_BONDS[["U.S. 6 per cents, 1867-8"]] <-
  function(date) data.frame(bond = c("us_6pct_1868_jan", "us_6pct_1868_jul"),
                            wgt = c(0.590, 0.410))

#' US 6 per cents 1881 weights derived from value issued
MATCH_BONDS[["U.S. 6 per cents, 1881"]] <-
  function(date) data.frame(bond = c("us_6pct_1881_jan", "us_6pct_1881_jul"),
                            wgt = c(0.089, 0.911))

MATCH_BONDS[["U.S. 5 per cents, 1874"]] <-
  function(date) data.frame(bond = "us_5pct_1874", wgt = 1)

MATCH_BONDS[["Virginia 6 per cents"]] <-
  function(date) make_bond_table(sprintf("virginia_6pct_%d", 1885:1890))

MATCH_BONDS[["Tennessee 6 per cents"]] <-
  function(date) make_bond_table(sprintf("tennessee_6pct_%d", 1885:1892))

MATCH_BONDS[["Indiana 6 per cents"]] <-
  function(date) make_bond_table("indiana_6pct_1881")

unmatched <- setdiff(unique(bankers$series), names(MATCH_BONDS))
if (length(unmatched)) {
  stop(sprintf("No entries for: %s", paste(unmatched, collapse = ", ")))
}

.data <-
  plyr::mdply(bankers,
              function(series, date, ...) {
                MATCH_BONDS[[as.character(series)]](date)
              }) %>%
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
                 series %in% c("U.S. 6 per cents, 1867-8",
                               "U.S. 6 per cents, 1881",
                               "U.S. 6 per cents, 1874")) %>%
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
  #filter(date >= as.Date("1862-1-1")) %>%
  ungroup() %>%
  group_by(date) %>%
  do(rfyields_date(.))

.data %<>% left_join(yields6, by = c("date", "bond"))

write_csv(.data, file = outfile)
