#' Bankers magazine
#'
#' Adds price in gold. Gold prices come from ``greenbacks_fill``.
source("R/init.R")

args <- commandArgs(TRUE)
dst <- args[1]

### depends: sources/data/bankers_magazine_govt_state_loans.csv data/greenbacks_fill.csv
src <- "sources/data/bankers_magazine_govt_state_loans.csv"
greenbacks_fill_file <- "data/greenbacks_fill.csv"

#' Southern states do not pay interest after 1861.
#' Assume that their prices are "clean"
#' And that they are just continually defaulting on next payment.
clean_series <- function(series, date, is_clean) {
    is_clean <- fill_na(is_clean)
    is_clean[(series %in% c("Missouri 6 per cents",
                           "Virginia 6 per cents",
                           "Georgia 6 per cents",
                           "Louisiana 6 per cents",
                           "Tennessee 6 per cents")) &
                 (date > as.Date("1861-1-1"))] <- 1
    is_clean
}

greenbacks <- (mutate(read_csv(greenbacks_fill_file),
                      date = as.Date(date, "%Y-%m-%d"),
                      gold_rate = 100 / mean)
               %>% select(date, gold_rate))
bankers <- mutate(read_csv(src),
                  date = as.Date(date, "%Y-%m-%d"))
bankers <- merge(bankers, greenbacks, by = "date", all.x = TRUE)
bankers$gold_rate[bankers$date < as.Date("1862-1-1")] <- 1
bankers <-
    (mutate(bankers,
            is_clean = clean_series(series, date, is_clean),
            price_currency = value,
            price_gold = price_currency / gold_rate,
            adjust_gold = fill_na(adjust_gold),
            adjust_currency = fill_na(adjust_currency))
     %>% select(-value))
write_csv(bankers, file = dst)
