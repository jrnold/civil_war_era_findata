#' Confederate Interest Rates for Bonds
source("R/init.R")

dst <- commandArgs(TRUE)[1]

### depends: sources/data/richmond_mkt.csv
src <- "sources/data/richmond_mkt.csv"
### depends: data/graybacks_fill.csv
graybacks_fill_file <- "data/graybacks_fill.csv"

create_series <- function(x, registered) {
  y <- rep(NA_character_, length(x))
  y[grepl("virginia", x) & registered] <- "virginia_reg"
  y[grepl("virginia", x) & ! registered] <- "virginia_coup"
  y[grepl("confederate_15mn", x)] <- "confederate_15mn"
  y[grepl("confederate_100mn", x)] <- "confederate_100mn"
  y[grepl("north_carolina_8", x)] <- "north_carolina_8pct"
  y[grepl("north_carolina_6_old", x)] <- "north_carolina_6pct_old"
  y[grepl("north_carolina_6_new", x)] <- "north_carolina_6pct_new"
  y[grepl("richmond", x)] <- "richmond"
  y[grepl("petersburg", x)] <- "petersburg"
  y
}

graybacks_fill <- read_csv(graybacks_fill_file) %>%
  mutate(date = as.Date(date)) %>%
  mutate(gold_rate = exp(log_price)) %>%
  select(date, gold_rate)

ASSETS_TOUSE = c("confederate_15mn", "confederate_100mn",
                 "virginia", "north_carolina", "richmond",
                 "petersburg")


richmond <- read_csv(src) %>%
  mutate(date = as.Date(date),
         newspaper_date = as.Date(date)) %>%
  filter(grepl(paste("(", ASSETS_TOUSE, ")", collapse = "|", sep = ""),
               asset)) %>%
  left_join(graybacks_fill, by = "date") %>%
  mutate(price_gold_low = price_low / gold_rate,
         price_gold_high = price_high / gold_rate,
         series = create_series(asset, registered)) %>%
  mutate(interest = ifelse(series %in% c("confederate_15mn", "confederate_100mn",
                                        "north_carolina_8pct"),
                          8, NA),
         interest = ifelse(series %in% c("north_carolina_6pct_old",
                                         "north_carolina_6pct_new",
                                         "virginia_coup", "virginia_reg",
                                         "petersburg", "richmond"),
                           6, interest),
         current_yield_gold = interest /
           (0.5 * (price_gold_low + price_gold_high)),
         current_yield_currency = interest / (0.5 * (price_low + price_high)))

write_csv(richmond, file = dst)
