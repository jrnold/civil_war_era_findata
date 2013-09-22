library("plyr")
library("reshape2")
library("RJSONIO")
library("doMC")
registerDoMC()

sysargs <- commandArgs(TRUE)
bankers_file <- sysargs[1]
bond_metadata_file <- sysargs[2]
outfile <- sysargs[3]

source("sources/scripts/R/finance.R")

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

MATCH_BONDS[["california_7pct_1870"]] <-
    function(date) {
        bonds <- grep("california_seven_1870", names(bond_metadata), value=TRUE)
        data.frame(bond = bonds, wgt = 1 / length(bonds))
    }

MATCH_BONDS[["california_7pct_1877"]] <-
    function(date) {
        bonds <- grep("california_seven_1877", names(bond_metadata), value=TRUE)
        data.frame(bond = bonds, wgt = 1 / length(bonds))
    }

MATCH_BONDS[["georgia_6pct"]] <-
    function(date) {
        bonds <- grep("georgia_six", names(bond_metadata), value=TRUE)
        data.frame(bond = bonds, wgt = 1 / length(bonds))
    }

## Indiana 5
##
## set of comparisons
##
## - Kentucky: 1869-72
## - Missouri: 1872
## - North Carolina: 1873
## - Pennsylvania: 1873
## - Virginia: 1885-1890
## - Tennessee: 1885-1892
## - Georgia: 1872
## yrs <-
## data.frame(yr= c(1869:1872, 1872, 1873, 1885:1890, 1885:1892, 1872),
##            wt = c(rep(0.25, 4), 1, 1, rep(1/6, 6), rep(1/8, 8), 1))
## dput(transform(ddply(yrs, "yr", summarise, wt = sum(wt)), wt = wt / sum(wt))$wt)
MATCH_BONDS[["indiana_5pct"]] <- 
    function(date) {
        bonds <- c("indiana_five_18690701", "indiana_five_18700701", "indiana_five_18710701", 
                   "indiana_five_18720701", "indiana_five_18730701", "indiana_five_18740701", 
                   "indiana_five_18750701", "indiana_five_18760701", "indiana_five_18770701", 
                   "indiana_five_18780701", "indiana_five_18790701", "indiana_five_18800701", 
                   "indiana_five_18810701", "indiana_five_18820701", "indiana_five_18830701", 
                   "indiana_five_18840701", "indiana_five_18850701", "indiana_five_18860701", 
                   "indiana_five_18870701", "indiana_five_18880701", "indiana_five_18890701", 
                   "indiana_five_18900701", "indiana_five_18910701", "indiana_five_18920701"
                   )
        wgts <- c(0.0416666666666667, 
                  0.0416666666666667, 0.0416666666666667, 0.375, 0.166666666666667, 0, # 1873
                  0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0,
                  0.0486111111111111, 0.0486111111111111, 0.0486111111111111, 0.0486111111111111, 0.0486111111111111,
                  0.0486111111111111, 0.0208333333333333, 0.0208333333333333)
        data.frame(bond = bonds, wgt = wgts)
    }

MATCH_BONDS[["kentucky_6pct"]] <-
    function(date) {
        bonds <- grep("kentucky_six", names(bond_metadata), value=TRUE)
        data.frame(bond = bonds, wgt = 1 / length(bonds))
    }

## Louisiana
##
## set of comparisons
##
## - Kentucky: 1869-72
## - Missouri: 1872
## - North Carolina: 1873
## - Pennsylvania: 1873
## - Virginia: 1885-1890
## - Tennessee: 1885-1892
## yrs <-
## data.frame(yr= c(1869:1872, 1872, 1873, 1885:1890, 1885:1892),
##            wt = c(rep(0.25, 4), 1, 1, rep(1/6, 6), rep(1/8, 8)))
## dput(transform(ddply(yrs, "yr", summarise, wt = sum(wt)), wt = wt / sum(wt))$wt)
MATCH_BONDS[["louisiana_6pct"]] <-
    function(date) {
        bonds <- sprintf("louisiana_six_%d0701", 1869:1892)
        wgts <- c(0.05, 
                  0.05, 0.05, 0.25, 0.2, 0, #1870
                  0, 0, 0, 0, 0, # 1875
                  0, 0, 0, 0, 0, # 1880
                  0.0583333333333333, 0.0583333333333333, 0.0583333333333333, 0.0583333333333333, 0.0583333333333333, # 1885
                  0.0583333333333333, 0.025, 0.025) #1890
        data.frame(bond = bonds, wgt = wgts)
    }


MATCH_BONDS[["missouri_6pct"]] <-
        function(date) {
            bonds <- grep("missouri_six", names(bond_metadata), value=TRUE)
            data.frame(bond = bonds, wgt = 1 / length(bonds))
        }

MATCH_BONDS[["north_carolina_6pct"]] <-
        function(date) {
            bonds <- grep("north_carolina_six", names(bond_metadata), value=TRUE)
            data.frame(bond = bonds, wgt = 1 / length(bonds))
        }


MATCH_BONDS[["ohio_6pct_1874"]] <- 
    function(date) data.frame(bond = "ohio_six_18740701", wgt = 1)

MATCH_BONDS[["ohio_6pct_1886"]] <- 
    function(date) data.frame(bond = "ohio_six_18860701", wgt = 1)

MATCH_BONDS[["pennsylvania_6pct"]] <-
        function(date) {
            bonds <- grep("pennsylvania_six", names(bond_metadata), value=TRUE)
            data.frame(bond = bonds, wgt = 1 / length(bonds))
        }


#' US 6 percent 1868 weights derived from value issued
MATCH_BONDS[["US_6pct_1868"]] <-
    function(date) data.frame(bond = c("us_sixes_18680101", "us_sixes_18680701"),
                              wgt = c(0.590, 0.410))

#' US 6 percent 1881 weights derived from value issued
MATCH_BONDS[["US_6pct_1881"]] <-
    function(date) data.frame(bond = c("us_sixes_18810101", "us_sixes_18810701"),
                              wgt = c(0.089, 0.911))

MATCH_BONDS[["US_5pct_1874"]] <-
    function(date) data.frame(bond = "us_fives_18740101", wgt = 1)

MATCH_BONDS[["virginia_6pct"]] <-
        function(date) {
            bonds <- grep("virginia_six", names(bond_metadata), value=TRUE)
            data.frame(bond = bonds, wgt = 1 / length(bonds))
        }

MATCH_BONDS[["tennessee_6pct"]] <-
    function(date) {
        bonds <- grep("tennessee_six", names(bond_metadata), value=TRUE)
        data.frame(bond = bonds, wgt = 1 / length(bonds))
    }

MATCH_BONDS[["indiana_6pct"]] <-
    function(date) {
        bonds <- grep("indiana_six", names(bond_metadata), value=TRUE)
        data.frame(bond = bonds, wgt = 1 / length(bonds))
    }

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

fields <- c("series", "date", "bond", "price_paper_dirty", "price_gold_dirty",
            "price_paper_clean", "price_gold_clean", "accrued", "current_yield",
            "yield", "maturity", "duration")

yields <- mdply(bankers, FUN, METADATA = bond_metadata, MATCH_BONDS=MATCH_BONDS,
                .parallel = TRUE)
yields <- arrange(yields, series, date, bond)
write.csv(yields[ , fields], file = outfile, row.names = FALSE, na="")
