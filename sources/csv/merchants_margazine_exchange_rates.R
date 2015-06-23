#' Cleaning Merchants' Magazine Exchange Rates
#'
source("R/init.R")

#' output
dst <- commandArgs(TRUE)[1]

#' Source files
#'
### depends: sources/data/merchants_exchange_rates.csv
src <- "sources/data/merchants_exchange_rates.csv"
### depends: data/greenbacks_fill.csv
greenbacks_fill_file <- "data/greenbacks_fill.csv"

greenbacks <- read_csv(greenbacks_fill_file) %>%
  mutate(date = as.Date(date),
         gold_rate = mean) %>%
  select(date, gold_rate)

#' Conversions
#'
#' - London quoted in premium assuming par is 1 pound sterling per 4.44 dollars
#'   Convert to 1 dollar per 100 pound sterling
#' - Paris is quoted in francs / dollars. Convert to 1 dollar per 100 francs.
.data <- read_csv(src) %>%
  mutate(date = as.Date(date)) %>%
  left_join(greenbacks, by = "date") %>%
  mutate(gold_rate = fill_na(gold_rate, 100) / 100,
         rate_low = ifelse(city == "Paris", 100 / rate_low, rate_low),
         rate_high = ifelse(city == "Paris", 100 / rate_high, rate_high),
         rate_low = ifelse(city == "London", rate_low * 4.44, rate_low),
         rate_high = ifelse(city == "London", rate_high * 4.44, rate_high),
         rate_low = ifelse(currency, rate_low * gold_rate, rate_low),
         rate_high = ifelse(currency, rate_high * gold_rate, rate_high),
         currency = as.integer(currency)
  ) %>%
  select(city, date, rate_low, rate_high, gold_rate,
         comment, url, issue)

write_csv(.data, file = dst)
