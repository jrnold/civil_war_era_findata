library("dplyr")
library("reshape2")
library("jsonlite")
source("sources/scripts/R/misc.R")
source("sources/scripts/R/finance.R")

sysargs <- commandArgs(TRUE)
merchants_file <- sysargs[1]
merchants_file <- "data/merchants_magazine_us_paper.csv"
bond_metadata_file <- sysargs[2]
bond_metadata_file <- "data/bond_metadata.json"
outfile <- sysargs[3]

#' Load prerequisite data
BOND_SERIES <- c("fives_1874",
                 sprintf("sixes_1881_%s", c("reg", "coup")),
                 sprintf("five_twenty_%s", c("reg", "coup")),
                 "ten_forty", "seven_thirties")

merchants <-
    (mutate(read.csv(merchants_file),
            date = as.Date(date, "%Y-%m-%d"))
     %>% select(date, series, adjust_gold, adjust_currency,
                price_gold, gold_rate, is_clean)
     %>% filter(!is.na(price_gold)))

merchants_bonds <-
    filter(merchants, series %in% BOND_SERIES)

bond_metadata <-
    fromJSON(bond_metadata_file,
             simplifyDataFrame = FALSE)

#' # Series -> BONDS weights
MATCH_BONDS <- list()

MATCH_BONDS[["fives_1874"]] <-
    function(date) make_bond_table("us_5pct_1874")

MATCH_BONDS[["sixes_1881_coup"]] <-
    function(date) {
        data.frame(bond =
                   sprintf("us_6pct_1881_%s", c("jan", "jul")),
                   wgt = c(0.089, 0.911))
    }

#' For the sixes of 1881 apply to Jan and July Sixes of 1881
MATCH_BONDS[["sixes_1881_reg"]] <-
    function(date) {
        data.frame(bond =
                   sprintf("us_6pct_1881_%s", c("jan", "jul")),
                   wgt = c(0.089, 0.911))
    }

#' The five-twenties are first quoted in Jan 1, 1865.
#' So assume that the five-twenties quoted are the five-twenties of 1864, not 1862 or 1865.
MATCH_BONDS[["five_twenty_coup"]] <-
    function(date) {
        make_bond_table_regex("^us_five_twenty_of_1864", bond_metadata)
    }

MATCH_BONDS[["ten_forty"]] <-
    function(date) {
        make_bond_table_regex("^us_ten_forty_", bond_metadata)
    }

MATCH_BONDS[["five_twenty_reg"]] <-
    function(date) {
        make_bond_table_regex("^us_five_twenty_of_1864", bond_metadata)
    }

MATCH_BONDS[["seven_thirties"]] <-
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

## for (i in 1:nrow(merchants_bonds)) {
##     print(i)
##     plyr::splat(match_series_to_bonds)(merchants_bonds[i, ])
## }

.data <- plyr::mdply(merchants_bonds, match_series_to_bonds)

## for (i in 1:nrow(.data)) {
##     print(i)
##     plyr::splat(make_yields_etc)(.data[i, ], bond_metadata = bond_metadata)
## }

.data2 <-
    (plyr::mdply(.data, make_yields_etc,
                 bond_metadata = bond_metadata)
     )


#'
#' One year old (Certificates of Indebtedness)
#'
#' Issued under the Act of March 1, 1862 (12 Stat 352).
#' Paid 6 percent interest on maturity (1 year after issue).
#' Act of March 3, 1863 (12 Stat 710) made the interest payable
#' in lawful money.n
#'
#' Quoted from 1862-3-26 to 1864-3-26. For yield calculates, I will assume that the interest is always payable in lawfult money rather than currency.
#'
#' - Bayley, [p. 81](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA81), [p. 158-159](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA158)
#' - De Knight, [p. 86-87](http://books.google.com/books?id=0cQmAQAAMAAJ&pg=PA86)
#' - Noll ? 
#' - Annual Report of the Treasury 1863, [p. 44-45](https://fraser.stlouisfed.org/docs/publications/treasar/AR_TREASURY_1863.pdf#page=52)
#'
oneyr_old <- plyr::ldply(c(6, 12),
                         function(mon) {
                             i <- mon / 12
                             (filter(merchants, series == "oneyr_old")
                              %>% mutate(bond = paste0("us_cert_indebt_1862_maturity_", mon, "_mon"),
                                         wgt = 0.5,
                                         price = price_gold + adjust_gold + (adjust_currency / gold_rate),
                                         price_clean = price,
                                         accrued_interest = NA,
                                         ytm = -log((price * gold_rate) / 106) / i,
                                         duration = i,
                                         convexity = i^2,
                                         maturity = i))
                         })

#'
#' One Year New (Treasury Notes of 1863)
#'
#' Issued under the Act of March 3, 1863.
#' Paid interest of 5 percent on maturity (1 year after issue).
#' Both principal and interest paid in lawful currency.
#'
#' These were quoted after 1863-4-25.
#' 
#' - Bayley, [p. 82-84](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA82),  [p. 161](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA161)
#' - De Knight, [p. 88-89](http://books.google.com/books?id=0cQmAQAAMAAJ&pg=PA88)
#' - Noll, Vol 8, [p. 304](http://www.franklinnoll.com/Vol_8.pdf#page=305)
#' - Annual Report of the Treasury 1865, [p. 52-53](https://fraser.stlouisfed.org/docs/publications/treasar/AR_TREASURY_1865.pdf#page=56)
#' 
oneyr_new <- plyr::ldply(c(0.5, 1),
                         function(mon) {
                             i <- mon / 12
                             (filter(merchants, series == "oneyr_new")
                              %>% mutate(bond = paste("us_one_year_notes_1863_mature_", mon, "_mon"),
                                         wgt = 0.5,
                                         price = price_gold + adjust_gold + (adjust_currency / gold_rate),
                                         price_clean = price,
                                         accrued_interest = NA,
                                         ytm = -log((price * gold_rate) / 105) / i,
                                         duration = i,
                                         convexity = i^2,
                                         maturity = i))
                     })

.data2 <-
    (do.call(plyr::rbind.fill,
             list(.data2, oneyr_old, oneyr_new))
     %>% select(-is_clean, -adjust_gold,
                -adjust_currency, -price_gold)
     %>% mutate(registered = as.integer(grepl("_reg$", series)))
     )

write.csv2(.data2, file = outfile)
