#' Yields on Greenbacks held until 1879-1-1
source(".init.R")

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
           actual_low = - log(low / 100) / actual_maturity,
           actual_high = - log(high / 100) / actual_maturity,
           actual_mean = - log(mean / 100) / actual_maturity,
           implied_maturity_low = gold_redemp_date(date, high / 100),
           implied_maturity_high = gold_redemp_date(date, low / 100),
           implied_maturity_mean = gold_redemp_date(date, mean / 100)) %>%
        select(date, matches("actual_"), matches("implied_"))

write_csv(greenback_yields, file = outfile)
