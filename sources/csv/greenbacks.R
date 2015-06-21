#' Preprocess ``greenbacks`` data
source("R/init.R")

### depends: sources/data/greenbacks.csv
src <- "sources/data/greenbacks.csv"
dst <- commandArgs(TRUE)[1]

greenbacks <- mutate(read_csv(src),
                     mean = exp_mean_log(low, high))
write_csv(greenbacks, file = dst)
