#' Preprocess ``greenbacks`` data
library(plyr)
args <- commandArgs(TRUE)
src <- args[1]
dst <- args[2]
greenbacks <- mutate(read.csv(src),
                     mean = exp(0.5 * (log(low) + log(high))))
write.csv(greenbacks, file = dst, row.names = FALSE)
