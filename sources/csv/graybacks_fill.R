#' Create greenbacks series with no missing values
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
  mutate(log_price = log(price / 100),
         date = as.Date(date)) %>%
  select(date, log_price)
graybacks_mccandless <- read_csv(srcs[2]) %>%
  mutate(date = as.Date(date),
         log_price = log(price)) %>%
  select(date, log_price)
graybacks_weidenmier <- read_csv(srcs[3]) %>%
  mutate(date = as.Date(date),
         log_price = log(price)) %>%
  select(date, log_price)

graybacks <-
  bind_rows(graybacks_ecm,
            graybacks_mccandless,
            graybacks_weidenmier) %>%
  group_by(date) %>%
  summarize(log_price = mean(log_price)) %>%
  right_join(data_frame(date = seq(as.Date("1861-1-1"),
                                   as.Date("1865-5-1"), by = 1)),
             by = "date")

graybacks_fill <-
  bind_rows(data_frame(date = seq(as.Date("1861-1-1"), as.Date("1865-4-26"), 1),
                       log_price = tsSmooth(StructTS(filter(graybacks,
                           date <= as.Date("1865-4-26")) %>%
                      `[[`("log_price"))) %>% `[`( , 1)),
            filter(graybacks, date > as.Date("1865-4-26")))

write_csv(graybacks_fill, file = dst)



write.csv(greenbacks, file=dst)
