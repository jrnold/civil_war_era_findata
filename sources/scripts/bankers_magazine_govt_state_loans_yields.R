library("dplyr")
library("reshape2")
library("jsonlite")
source("sources/scripts/R/finance.R")
source("sources/scripts/R/misc.R")

sysargs <- commandArgs(TRUE)
bankers_file <- sysargs[1]
#bankers_file <- "data/bankers_magazine_govt_state_loans.csv"
bond_metadata_file <- sysargs[2]
#bond_metadata_file <- "data/bond_metadata.json"
outfile <- sysargs[3]

#' Load prerequisite data
bankers <-
    (mutate(read.csv(bankers_file),
            date = as.Date(date, "%Y-%m-%d"))
     %.% select(date, series, is_clean, adjust_gold,
                adjust_paper, price_gold, gold_rate)
     %.% filter(!is.na(price_gold)))

bond_metadata <-
    fromJSON(bond_metadata_file,
             simplifyDataFrame = FALSE)



MATCH_BONDS <- list()

MATCH_BONDS[["california_7pct_1870"]] <-
    function(date) make_bond_table("california_7pct_1870")

MATCH_BONDS[["california_7pct_1877"]] <-
    function(date) make_bond_table("california_7pct_1877")    

#'
#' The are many possible values of the Georgia issue. Denny & Co list 1868-1887.
#' However, the table in the Bankers' Magazine in 1857 lists 1872 as the only redemption date.
#' Assume that Bankers' continues to quote only the 1872 issue.
MATCH_BONDS[["georgia_6pct"]] <-
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

MATCH_BONDS[["indiana_5pct"]] <- function(date) indiana_lookup

#'
#' The bond metadata includes years 1868-1885, but the table of bonds in http://books.google.com/books?id=g2QmAQAAIAAJ&pg=PA332 only lists 1869-72.
MATCH_BONDS[["kentucky_6pct"]] <-
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
MATCH_BONDS[["louisiana_6pct"]] <- function(date) louisiana_lookup

#' Denny and Co list 1868-1890, but [Bankers' Magazine 1857](http://books.google.com/books?id=g2QmAQAAIAAJ&pg=PA332) lists only 1872.
MATCH_BONDS[["missouri_6pct"]] <-
        function(date) make_bond_table("missouri_6pct_1872") 

MATCH_BONDS[["north_carolina_6pct"]] <-
        function(date) make_bond_table("north_carolina_6pct_1873")

MATCH_BONDS[["ohio_6pct_1874"]] <-
    function(date) make_bond_table("ohio_6pct_1874")

MATCH_BONDS[["ohio_6pct_1886"]] <- 
    function(date) make_bond_table("ohio_6pct_1886")

MATCH_BONDS[["pennsylvania_5pct"]] <-
    function(date) make_bond_table("pennsylvania_5pct_1871")

#' US 6 percent 1868 weights derived from value issued
MATCH_BONDS[["US_6pct_1868"]] <-
    function(date) data.frame(bond = c("us_6pct_1868_jan", "us_6pct_1868_jul"),
                              wgt = c(0.590, 0.410))

#' US 6 percent 1881 weights derived from value issued
MATCH_BONDS[["US_6pct_1881"]] <-
    function(date) data.frame(bond = c("us_6pct_1881_jan", "us_6pct_1881_jul"),
                              wgt = c(0.089, 0.911))

MATCH_BONDS[["US_5pct_1874"]] <-
    function(date) data.frame(bond = "us_5pct_1874", wgt = 1)

MATCH_BONDS[["virginia_6pct"]] <-
        function(date) make_bond_table(sprintf("virginia_6pct_%d", 1885:1890))

MATCH_BONDS[["tennessee_6pct"]] <-
    function(date) make_bond_table(sprintf("tennessee_6pct_%d", 1885:1892))

MATCH_BONDS[["indiana_6pct"]] <-
    function(date) make_bond_table("indiana_6pct_1881")

unmatched <- setdiff(unique(bankers$series), names(MATCH_BONDS))
if (length(unmatched)) {
    stop(sprintf("No entries for: %s", paste(unmatched, collapse = ", ")))
}

.data <- plyr::mdply(bankers,
                     function(series, date, ...) {
                         MATCH_BONDS[[as.character(series)]](date)
                     })

make_yields_etc <- 
    function(date, bond, gold_rate, price_gold, adjust_gold, adjust_paper, is_clean, ..., bond_metadata)
{
    metadata <- bond_metadata[[as.character(bond)]]
    if ("issue_date" %in% metadata) {
        issue_date <- metadata[["issue_date"]]
    } else issue_date <- NULL
    if (! is.null(issue_date) && ! is.na(issue_date)) {
        issue_date <- as.Date(issue_date, format = "%Y-%m-%d")
    }
    cashflows <-
        mutate(plyr::ldply(metadata[["cashflows"]],
                           function(x) as.data.frame(x)),
               date = as.Date(date, format="%Y-%m-%d"))
    cashflows <- gold_cashflows(cashflows, gold_rate)
    accrued <- accrued_interest(date, cashflows, issue_date)
    price_quoted <- price_currency
    price <- price_gold + adjust_gold + adjust_paper / gold_rate
    if (! is_clean) {
        price_clean <- price - accrued
    } else {
        price_clean <- price
        price <- price_clean + accrued
    }
    yields <- yield_to_maturity2(price, date, cashflows)
    data.frame(price = price,
               price_listed = price_listed,
               price_clean = price_clean,
               accrued_interest = accrued,
               ytm = as.numeric(yields),
               duration = attr(yields, "duration"),
               convexity = attr(yields, "convexity"),
               maturity = attr(yields, "maturity"))
}

## for (i in 1:nrow(.data)) {
##     plyr::splat(make_yields_etc)(.data[i, ], bond_metadata = bond_metadata)
## }
     
.data2 <-
    (plyr::mdply(.data, make_yields_etc,
                bond_metadata = bond_metadata)
     %.% select(-is_clean, -adjust_gold,
                -adjust_paper, -price_gold)
     )

write.csv2(.data2, file = outfile)
