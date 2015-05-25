library("plyr")
library("reshape2")
source("sources/scripts/R/misc.R")

sysargs <- commandArgs(TRUE)
infile <- sysargs[1]
outfile <- sysargs[2]

data1 <- read_csv(infile)
data2 <- ddply(data1, c("series", "date"),
             function(x) {
                 touse <- setdiff(names(x), c("bond", "series", "date", "wgt"))
                 wgt <- x[["wgt"]]
                 fun <- function(y) weighted.mean(y, wgt)
                 colwise(fun, touse)(x)
             })

write_csv(data2, file = outfile)
