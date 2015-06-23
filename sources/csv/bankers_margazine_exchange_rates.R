#' Cleaning Bankers' Magazine Exchange Rates
#'
source("R/init.R")

#' output
dst <- commandArgs(TRUE)[1]

#' Source files
#'
### depends: sources/data/bankers_magazine_exchange_rates.csv
src <- "sources/data/bankers_magazine_exchange_rates.csv"
### depends: data/greenbacks_fill.csv
greenbacks_fill_file <- "data/greenbacks_fill.csv"

greenbacks <- read_csv(greenbacks_fill_file) %>%
  mutate(date = as.Date(date),
         gold_rate = mean) %>%
  select(date, gold_rate)

bankers <- read_csv(src) %>%
  mutate(issue = as.Date(issue, format = "%m/%d/%Y"),
         date = as.Date(date)) %>%
  left_join(greenbacks, by = "date") %>%
  mutate(gold_rate = fill_na(gold_rate, 100) / 100,
         rate_low = ifelse(city == "Paris", 100 / rate_high, rate_low),
         rate_high = ifelse(city == "Paris", 100 / rate_low, rate_high),
         rate_low = ifelse(city == "London", rate_low * 4.44, rate_low),
         rate_high = ifelse(city == "London", rate_high * 4.44, rate_high),
         rate_low = rate_low * gold_rate,
         rate_high = rate_high * gold_rate,
         type = ifelse(city == "London", type, "")
  ) %>%
  select(city, type, date, rate_low, rate_high,
         gold_rate, url, issue)

write_csv(bankers, file = dst)
