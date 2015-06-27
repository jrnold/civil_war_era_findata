#' Implied yields and maturities of greenbacks
source("R/init.R")

### depends: data/greenbacks.csv
infile <- "data/greenbacks.csv"
outfile <- commandArgs(TRUE)[1]

#' The actual maturity date?
MATURITY_DATE <- as.Date("1879-1-1")

greenbacks <-
  mutate(read_csv(infile),
         date = as.Date(date)) %>%
  filter(! is.na(mean))

#' What is the discount rate that fits the actual redemption rate the best?
best_r <- function(r) {
  actual <- difftime_years(as.Date("1879-1-1"), greenbacks$date)
  implied <- - log(greenbacks$mean / 100) / r
  sum((actual - implied) ^ 2)
}
rstar <- optimize(best_r, c(0, 1))$minimum

greenback_yields <-
    mutate(greenbacks,
           actual_maturity = difftime_years(MATURITY_DATE, date),
           # yield to maturity using the actual maturity date
           yield_actual_low = - log(low / 100) / actual_maturity,
           yield_actual_high = - log(high / 100) / actual_maturity,
           yield_actual_mean = - log(mean / 100) / actual_maturity,
           # 4%
           implied_maturity_low_4pct = gold_redemp_time(100 / high, r = 0.04),
           implied_maturity_high_4pct = gold_redemp_time(100 / low, r = 0.04),
           implied_maturity_mean_4pct = gold_redemp_time(100 / mean, r = 0.04),
           # 5%
           implied_maturity_low_5pct = gold_redemp_time(100 / high, r = 0.05),
           implied_maturity_high_5pct = gold_redemp_time(100 / low, r = 0.05),
           implied_maturity_mean_5pct = gold_redemp_time(100 / mean, r = 0.05),
           # 6%
           implied_maturity_low_6pct = gold_redemp_time(100 / high, r = 0.06),
           implied_maturity_high_6pct = gold_redemp_time(100 / low, r = 0.06),
           implied_maturity_mean_6pct = gold_redemp_time(100 / mean, r = 0.06),
           # optimal r
           implied_maturity_low_rstar = gold_redemp_time(100 / high, r = rstar),
           implied_maturity_high_rstar = gold_redemp_time(100 / low, r = rstar),
           implied_maturity_mean_rstar = gold_redemp_time(100 / mean, r = rstar)
           ) %>%
        select(date, matches("actual_"), matches("implied_"))



write_csv(greenback_yields, file = outfile)
