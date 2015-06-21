source("R/init.R")

dst <- commandArgs(TRUE)[1]

### depends: sources/data/bankers_magazine_govt_bonds_quotes_in_text.csv data/greenbacks_fill.csv
src <- "sources/data/bankers_magazine_govt_bonds_quotes_in_text.csv"
greenbacks_fill_file <- "data/greenbacks_fill.csv"

greenbacks <- (mutate(read_csv(greenbacks_fill_file),
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
