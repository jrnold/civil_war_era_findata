#' Create graybacks series with no missing values
#'
#' Uses greenback data from ``greeenbacks``. Fills in missing
#' data using the smoothed values of a ``StructTS`` local level model.
source("R/init.R")

### depends: data/grayback_ecm.csv data/grayback_mccandless.csv data/grayback_weidenmier_2002.csv
srcs <- c("data/grayback_ecm.csv",
          "data/grayback_mccandless.csv",
          "data/grayback_weidenmier_2002.csv")
dst <- commandArgs(TRUE)[1]

graybacks_ecm <- read_csv(srcs[1]) %>%
  select(- hard_to_read) %>%
  mutate(src = "ECM",
         price = price / 100)
graybacks_mccandless <- read_csv(srcs[2]) %>%
  mutate(src = "McCandless")
graybacks_weidenmier <- read_csv(srcs[3]) %>%
  select(- wilmington, - imputed) %>%
  mutate(src = "Weidenmier")

graybacks <-
  bind_rows(graybacks_ecm,
            graybacks_mccandless,
            graybacks_weidenmier) %>%
  mutate(date = as.Date(date),
         yield_1879 = - log(price) / difftime_years(date, as.Date("1879-1-1")),
         implied_maturity_5pct = gold_redemp_date(date, price, r = 0.05),
         implied_maturity_6pct = gold_redemp_date(date, price, r = 0.06),
         implied_maturity_7pct = gold_redemp_date(date, price, r = 0.07),
         implied_maturity_8pct = gold_redemp_date(date, price, r = 0.08)) %>%
  select(date, src, everything())

write_csv(graybacks, file = dst)
