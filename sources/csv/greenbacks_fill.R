#' Create greenbacks series with no missing values
#'
#' Uses greenback data from ``greeenbacks``. Fills in missing
#' data using the smoothed values of a ``StructTS`` local level model.
source("R/init.R")

dst <- commandArgs(TRUE)[1]
src <- "data/greenbacks.csv"

greenbacks <-
  mutate(read_csv(src),
         mean = exp(ts_interpolate(log(mean), "trend")),
         low = exp(ts_interpolate(log(low), "trend")),
         high = exp(ts_interpolate(log(high), "trend")))
greenbacks$comment <- NULL
write_csv(greenbacks, file=dst)

### depends: data/greenbacks.csv
