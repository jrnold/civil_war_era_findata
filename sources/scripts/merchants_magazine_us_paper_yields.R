library("plyr")
library("reshape2")
library("RJSONIO")
library("doMC")
registerDoMC()

sysargs <- commandArgs(TRUE)
merchants_file <- sysargs[1]
bond_metadata_file <- sysargs[2]
outfile <- sysargs[3]

source("sources/scripts/R/finance.R")

#' Load prerequisite data
merchants <- mutate(read.csv(merchants_file),
                  date = as.Date(date, "%Y-%m-%d"))
for (i in c("issue", "url", "volume")) {
    merchants[[i]] <- NULL
}

con <- file(bond_metadata_file, "r")
bond_metadata <- fromJSON(con)
close(con)

NOTES <- c("aug_demand", "gold", "gold_low", "gold_high",
           "oneyr_old", "oneyr_new")

#' # Series -> BONDS weights
MATCH_BONDS <- list()

MATCH_BONDS[["fives_1874"]] <-
    function(date) {
        bonds <- "us_fives_18740101"
        data.frame(bond = bonds, wgt = 1 / length(bonds))
    }

## MATCH_BONDS[["oneyr_old"]] <-
##     function(date) {
##         data.frame(bond = bonds, wgt = 1 / length(bonds))
##     }

## MATCH_BONDS[["oneyr_new"]] <-
##     function(date) {
##         data.frame(bond = bonds, wgt = 1 / length(bonds))
##     }

MATCH_BONDS[["sixes_1881_coup"]] <-
    function(date) {
        bonds <- grep("us_sixes_1881", names(bond_metadata),
                      value = TRUE)
        wgts <- c(0.089, 0.911)
        data.frame(bond = bonds, wgt = wgts)
    }

MATCH_BONDS[["sixes_1881_reg"]] <-
    function(date) {
        bonds <- grep("us_sixes_1881", names(bond_metadata),
                      value = TRUE)
        wgts <- c(0.089, 0.911)
        data.frame(bond = bonds, wgt = wgts)
    }

MATCH_BONDS[["five_twenty_coup"]] <-
    function(date) {
        bonds <- grep("us_five_twenty", names(bond_metadata), value=TRUE)
        data.frame(bond = bonds, wgt = 1 / length(bonds))
    }

MATCH_BONDS[["five_twenty_reg"]] <-
    function(date) {
        bonds <- grep("us_five_twenty", names(bond_metadata), value=TRUE)
        data.frame(bond = bonds, wgt = 1 / length(bonds))
    }

MATCH_BONDS[["ten_forty"]] <-
    function(date) {
        bonds <- grep("us_ten_forty", names(bond_metadata), value=TRUE)
        data.frame(bond = bonds, wgt = 1 / length(bonds))
    }


MATCH_BONDS[["seven_thirties"]] <-
    function(date) {
        if (date < as.Date("1864-8-15")) {
            data.frame(bond = 
                       c("us_seven_thirties_18640819",
                         "us_seven_thirties_18641001"),
                       wgt = c(0.5, 0.5))
        } else if (date >= as.Date("1864-8-15")
                   & date < as.Date("1864-8-19")) {
            data.frame(bond =
                       c("us_seven_thirties_18640819",
                         "us_seven_thirties_18641001",
                         "us_seven_thirties_18670815"),
                       wgt = c(0.168, 0.168, 0.664))
        } else if (date >= as.Date("1864-8-19")
                   & date < as.Date("1864-10-1")) {
            data.frame(bond =
                       c("us_seven_thirties_18641001",
                         "us_seven_thirties_18670815"),
                       wgt = c(0.202, 0.798))
        } else if (date >= as.Date("1864-10-1")) {
            data.frame(bond = c("us_seven_thirties_18670815"),
                       wgt = 1)
        }
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

#' U.S. Certificates of Indebtedness of 1863
#' Pay 6 percent in specie.
#' Until Act of March 3, 1863, changed interest payment to specie.
oneyr_old1 <-
    mutate(subset(merchants,
                  series == "oneyr_old"
                  & date < as.Date("1863-3-3"),
                  c(series, date, price_paper, price_gold)),
           bond = "us_one_year_notes_1863",
           price_paper_dirty = price_paper,
           price_gold_dirty = price_gold,
           price_paper = NULL,
           price_gold = NULL,
           price_paper_clean = price_paper_dirty,
           price_gold_clean = price_gold_dirty,
           current_yield = 6 / price_gold_dirty,
           yield = - log(price_gold_dirty / 100),
           maturity = 1,
           duration = 1)

oneyr_old2 <-
    mutate(subset(merchants,
                  series == "oneyr_old"
                  & date >= as.Date("1863-3-3"),
                  c(series, date, price_paper, price_gold)),
           bond = "us_one_year_notes_1863",
           price_paper_dirty = price_paper,
           price_gold_dirty = price_gold,
           price_paper = NULL,
           price_gold = NULL,
           price_paper_clean = price_paper_dirty,
           price_gold_clean = price_gold_dirty,
           interest = 6 * (price_gold_clean / price_paper_clean),
           current_yield = interest / price_gold_dirty,
           yield = - log(price_paper_dirty / (100 + interest)),
           maturity = 1,
           duration = 1)

oneyr_new <-
    mutate(subset(merchants, series == "oneyr_new",
                  c(series, date, price_paper, price_gold)),
           bond = "us_one_year_notes_1863",
           price_paper_dirty = price_paper,
           price_gold_dirty = price_gold,
           price_paper = NULL,
           price_gold = NULL,
           price_paper_clean = price_paper_dirty,
           price_gold_clean = price_gold_dirty,
           current_yield = 5 / price_paper_dirty,
           yield = - log(price_paper_dirty / 105),
           maturity = 1,
           duration = 1)

FUN <- function(series, date, price_gold, price_paper, ..., METADATA, MATCH_BONDS) {
    bonds <- MATCH_BONDS[[as.character(series)]](date)
    bonds$series <- series
    bonds$date <- date
    bonds$price_gold_dirty <- price_gold
    bonds$price_paper_dirty <- price_paper
    mdply(bonds, FUN2, METADATA=bond_metadata)
}

#' Yields for  coupon bonds
coupons <- mdply(subset(merchants,
                        ! series %in% NOTES),
                 FUN, METADATA = bond_metadata,
                MATCH_BONDS=MATCH_BONDS)

fields <- c("series", "date", "bond", "price_paper_dirty", "price_gold_dirty",
            "price_paper_clean", "price_gold_clean", "accrued", "current_yield",
            "yield", "maturity", "duration")
yields <- rbind.fill(coupons,
                     oneyr_new, oneyr_old1, oneyr_old2)

yields <- arrange(yields, series, date, bond)
write.csv(yields[ , fields], file = outfile,
          row.names = FALSE, na="")
