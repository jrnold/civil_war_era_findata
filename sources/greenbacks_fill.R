#' Create greenbacks series with no missing values
#'
#' Uses greenback data from ``greeenbacks``. Fills in missing
#' data using the smoothed values of a ``StructTS`` local level model.
source("sources/finance.R")
library(plyr)

args <- commandArgs(TRUE)
src <- args[1]
dst <- args[2]

greenbacks <-
  mutate(read.csv(src),
         mean = exp(ts_interpolate(log(mean), "trend")),
         low = exp(ts_interpolate(log(low), "trend")),
         high = exp(ts_interpolate(log(high), "trend")))
greenbacks$comment <- NULL
write.csv(greenbacks, file=dst, row.names=FALSE)
