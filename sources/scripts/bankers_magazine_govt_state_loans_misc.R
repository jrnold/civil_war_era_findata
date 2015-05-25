library("dplyr")
source("sources/scripts/R/misc.R")

args <- commandArgs(TRUE)
src <- args[1]
dst <- args[2]

greenbacks <- (mutate(read_csv("data/greenbacks_fill.csv"),
                      date = as.Date(date, "%Y-%m-%d"),
                      gold_rate = 100 / mean)
               %>% select(date, gold_rate))

bankers <- read_csv(src) %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  left_join(greenbacks, by = "date") %>%
  rename(price_currency_low = low_price,
         price_currency_high = high_price) %>%
  mutate(gold_rate  = ifelse(date < as.Date("1862-1-1"), 1, gold_rate),
         price_gold_low = price_currency_low / gold_rate,
         price_gold_high = price_currency_high / gold_rate,
         price_gold = exp(0.5 * (log(price_gold_low) +
                                        log(price_gold_high))),
         price_currency = exp(0.5 * (log(price_currency_low) +
                                        log(price_currency_high))),
         current_yield = interest / price_gold)
write_csv(bankers, file = dst)
