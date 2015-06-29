#' Cleaning Bankers' Magazine Exchange Rates
#'
source("R/init.R")
library("assertthat")

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
  mutate(issue = as.Date(issue),
         date = as.Date(date)) %>%
  left_join(greenbacks, by = "date") %>%
  mutate(gold_rate = fill_na(gold_rate, 100) / 100,
         # Paris
         rate_low = ifelse(city == "Paris", 100 / rate_low, rate_low),
         rate_high = ifelse(city == "Paris", 100 / rate_high, rate_high),
         # London
         rate_low = ifelse(city == "Londons", rate_low * 4.44, rate_low),
         rate_high = ifelse(city == "London", rate_high * 4.44, rate_high),
         # Convert to gold
         rate_low = rate_low * gold_rate,
         rate_high = rate_high * gold_rate,
         type = ifelse(city == "London", type, "")
  ) %>%
  select(city, type, date, rate_low, rate_high,
         gold_rate, url, issue)


## #' Checks on the data
## assert_that(all(bankers$rate_low <= bankers$rate_high))

## #'
## bankers_data_range <-
##   group_by(bankers, city) %>%
##   summarize(rate_low = min(rate_low), rate_high = max(rate_high)) %>%
##   left_join(
##   bind_rows(data_frame(city = "Amsterdam", rate_low_ = 30, rate_high_ = 50),
##             data_frame(city = "Bremen", rate_low_ = 65, rate_high_ = 85),
##             data_frame(city = "Frankfort", rate_low_ = 35, rate_high_ = 45),
##             data_frame(city = "Hamburg", rate_low_ = 30, rate_high_ = 40),
##             data_frame(city = "London", rate_low_ = 415, rate_high_ = 515),
##             data_frame(city = "Paris", rate_low_ = 15, rate_high_ = 25)),
##   by = c("city"))

## assert_that(all(bankers_data_range$rate_low_ < bankers_data_range$rate_low))
## assert_that(all(bankers_data_range$rate_high_ > bankers_data_range$rate_high))
assert_that(all(bankers$date <= as.Date("1863-12-24")))
assert_that(all(bankers$date >= as.Date("1859-05-26")))

write_csv(bankers, file = dst)
