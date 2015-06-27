#' Confederate Interest Rates for Bonds
source("R/init.R")

dst <- commandArgs(TRUE)[1]

### depends: sources/data/richmond_mkt.csv
src <- "sources/data/richmond_mkt.csv"

graybacks <- read_csv(src) %>%
  mutate(date = as.Date(date)) %>%
  filter(tolower(asset) %in% c("gold", "specie")) %>%
  select(date, price_low, price_high, newspaper_date, description,
         original_text, url)

write_csv(graybacks, file = dst)
