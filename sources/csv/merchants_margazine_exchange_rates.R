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

# ggplot(.data %>% mutate(rate = 0.5 * (rate_low + rate_high)),
#        aes(x = date, y = rate,
#            ymin = rate_low,
#            ymax = rate_high)) +
#   geom_pointrange() +
#   facet_wrap(~ city, scales = "free_y")
#
# Checked London, Berlin, Frankfort, Hamurg
# .data_data_range <-
#   group_by(.data, city) %>%
#   summarize(rate_low = min(rate_low), rate_high = max(rate_high)) %>%
#   left_join(
#     bind_rows(data_frame(city = "Amsterdam", rate_low_ = 30, rate_high_ = 50),
#               data_frame(city = "Bremen", rate_low_ = 65, rate_high_ = 85),
#               data_frame(city = "Frankfort", rate_low_ = 35, rate_high_ = 45),
#               data_frame(city = "Hamburg", rate_low_ = 30, rate_high_ = 40),
#               data_frame(city = "London", rate_low_ = 415, rate_high_ = 515),
#               data_frame(city = "Paris", rate_low_ = 15, rate_high_ = 25)),
#     by = c("city"))
#
# #' Checks on the data
# assert_that(all(.data$rate_low <= .data$rate_high))
#
# assert_that(all(.data_data_range$rate_low_ < .data_data_range$rate_low))
# assert_that(all(.data_data_range$rate_high_ > .data_data_range$rate_high))
assert_that(all(.data$date <= as.Date("1866-01-01")))
assert_that(all(.data$date >= as.Date("1860-01-01")))


write_csv(.data, file = dst)
