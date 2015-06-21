#' Create greenbacks series with no missing values
#'
#' Uses greenback data from ``greeenbacks``. Fills in missing
#' data using the smoothed values of a ``StructTS`` local level model.
source("R/init.R")

src <- "data/greenbacks.csv"
dst <- commandArgs(TRUE)[1]

greenbacks <-
  mutate(read_csv(src),
         mean = exp(ts_interpolate(log(mean), "trend")),
         low = exp(ts_interpolate(log(low), "trend")),
         high = exp(ts_interpolate(log(high), "trend")))
greenbacks$comment <- NULL
write_csv(greenbacks, file=dst)

### depends: data/greenbacks.csv
