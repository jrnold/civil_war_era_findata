library("dplyr")
library("reshape2")
library("jsonlite")
source("sources/scripts/R/finance.R")
source("sources/scripts/R/misc.R")

sysargs <- commandArgs(TRUE)
bankers_file <- sysargs[1]
bond_metadata_file <- sysargs[2]
outfile <- sysargs[3]

#' Load prerequisite data
bankers <- mutate(read.csv(bankers_file),
                  date = as.Date(date, "%Y-%m-%d"))
for (i in c("issue", "url", "volume")) {
    bankers[[i]] <- NULL
}

con <- file(bond_metadata_file, "r")
bond_metadata <- fromJSON(con)
close(con)

#' # Series -> BONDS weights
MATCH_BONDS <- list()

make_bond_table <- function(bonds) {
    data.frame(bond = bonds, wgt = 1 / length(bonds))
}

make_bond_table_regex <- function(pattern, metadata) {
    bonds <- grep(pattern, names(metadata), value = TRUE)
    make_bond_table(regex)
}

make_bond_table_dist <- function(pattern, .list, prior_yrs = NULL, prior_n = 1) {
    .data <- plyr::ldply(.list, function(x) data.frame(year = x, wgt = 1 / length(x)))
    if (is.null(prior_yrs)) {
        minyr <- min(.data$year)
        maxyr <- max(.data$year)
        prior_yrs <- seq(minyr, maxyr, by = 1)
    } 
    .prior <- data.frame(year = prior_yrs, wgt = 1 / length(prior_yrs) * prior_n)
    (group_by(plyr::rbind.fill(.data, .prior), "year")
     %.% summarise(wgt = sum(wgt))
     %.% mutate(wgt = wgt / sum(wgt),
                bond = sprintf(pattern, year))
     %.% select(bond, wgt))
}

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
    function(date) make_bond_table("kentucky_6pct_%d", 1869:1872)

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
#'  dput(transform(ddply(yrs, "yr", summarise, wt = sum(wt)), wt = wt / sum(wt))$wt)

louisinia_lookup <- 
    make_bond_table_dist("louisinia_6pct_%d",
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
    function(date) data.frame(bond = c("us_sixes_1868_jan", "us_sixes_1868_jul"),
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

#' Calculate accrued interest
#'
#' Return the accrued interest at date.
accrued_interest <- function(cashflows, issue_date, ncoupons, interest, date, face=100, ...) {
    datelist <- as.Date(sort(c(issue_date, cashflows$date)), as.Date("1970-1-1"))
    lastcoupon <- prev_date(as.Date(date), datelist)
    factor <- difftime_30_360(date, lastcoupon) / 360
    (factor * interest * face)
}
#' # Generate Yields
FUN2 <- function(bond, date, price_gold_dirty, price_paper_dirty, series, ..., METADATA) {
    bond <- as.character(bond)
    searchint <- c(-100, 100)
    metadata <- METADATA[[bond]]
    cashflows <- mutate(data.frame(metadata$cashflows),
                        date = as.Date(date, "%Y-%m-%d"))
    cashflows2 <- cashflows[cashflows$date > date, , drop=FALSE]
    issue_date <- metadata$issue_date
    if (!is.null(issue_date)) issue_date <- as.Date(issue_date, "%Y-%m-%d")
    ncoupons <- length(metadata$periods)
    interest <- metadata$interest
    gold_2_paper <- price_paper_dirty / price_gold_dirty
    accrued <- accrued_interest(cashflows, issue_date, ncoupons, interest, date)
    price_gold_clean <- price_gold_dirty - accrued
    price_paper_clean <- price_paper_dirty - gold_2_paper * accrued
    ## yield to maturity
    m <- difftime_30_360(cashflows2$date, date) / 360
    if (!is.na(price_gold_dirty)) {
        ytm <- try(ytm(c(-price_gold_dirty, cashflows2$amount), c(0, m), searchint = searchint))
        if (is(ytm, "try-error")) {
            print(sprintf("%s %s %s %f", series, bond, date, price_gold_dirty))
            ytm <- NA
        }
        duration <- macaulay_duration(cashflows2$amount, m, ytm, price_gold_clean)
    } else {
        ytm <- NA
        duration <- NA
    }
    ## ## Duration
    ## duration <- NA
    ## Current yield
    current_yield <- (interest * 100) / price_gold_clean
    ret <- 
        data.frame(accrued = accrued,
                   price_gold_clean = price_gold_clean,
                   price_paper_clean = price_paper_clean,
                   yield = ytm,
                   current_yield = current_yield,
                   duration = duration,
                   maturity = max(m))
    ret
}

FUN <- function(series, date, price_gold, price_paper, ..., METADATA, MATCH_BONDS) {
    bonds <- MATCH_BONDS[[as.character(series)]](date)
    bonds$series <- series
    bonds$date <- date
    bonds$price_gold_dirty <- price_gold
    bonds$price_paper_dirty <- price_paper
    mdply(bonds, FUN2, METADATA=bond_metadata)
}

fields <- c("series", "date", "bond", "wgt", "price_paper_dirty", "price_gold_dirty",
            "price_paper_clean", "price_gold_clean", "accrued", "current_yield",
            "yield", "maturity", "duration")

yields <- mdply(bankers, FUN, METADATA = bond_metadata, MATCH_BONDS=MATCH_BONDS,
                .parallel = TRUE)
yields <- arrange(yields, series, date, bond)
write.csv2(yields[ , fields], file = outfile)
