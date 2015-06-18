#' Implied yields and maturities of greenbacks
source("R/.init.R")

args <- commandArgs(TRUE)
infile <- args[1]
outfile <- args[2]

greenbacks <-
    mutate(read_csv(infile),
           date = as.Date(date))

MATURITY_DATE <- as.Date("1879-1-1")

greenback_yields <-
    mutate(greenbacks,
           actual_maturity = difftime_years(MATURITY_DATE, date),
           yield_actual_low = - log(low / 100) / actual_maturity,
           yield_actual_high = - log(high / 100) / actual_maturity,
           yield_actual_mean = - log(mean / 100) / actual_maturity,
           implied_maturity_low_5pct = gold_redemp_date(date, high / 100, r = 0.05),
           implied_maturity_high_5pct = gold_redemp_date(date, low / 100, r = 0.05),
           implied_maturity_mean_5pct = gold_redemp_date(date, mean / 100, r = 0.05),
           implied_maturity_low_4pct = gold_redemp_date(date, high / 100, r = 0.04),
           implied_maturity_high_4pct = gold_redemp_date(date, low / 100, r = 0.04),
           implied_maturity_mean_4pct = gold_redemp_date(date, mean / 100, r = 0.04),
           implied_maturity_low_6pct = gold_redemp_date(date, high / 100, r = 0.06),
           implied_maturity_high_6pct = gold_redemp_date(date, low / 100, r = 0.06),
           implied_maturity_mean_6pct = gold_redemp_date(date, mean / 100, r = 0.06)) %>%
        select(date, matches("actual_"), matches("implied_"))

write_csv(greenback_yields, file = outfile)
