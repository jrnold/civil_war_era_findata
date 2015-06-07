#' Preprocess ``greenbacks`` data
source(".init.R")
args <- commandArgs(TRUE)
src <- args[1]
dst <- args[2]
greenbacks <- mutate(read_csv(src),
                     mean = exp_mean_log(low, high))
write_csv(greenbacks, file = dst)
