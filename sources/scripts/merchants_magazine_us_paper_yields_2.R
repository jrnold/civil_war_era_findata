library("plyr")
library("reshape2")
library("RJSONIO")
library("doMC")
registerDoMC()

sysargs <- commandArgs(TRUE)
infile <- sysargs[1]
outfile <- sysargs[2]

data1 <- read.csv(infile)
data2 <- ddply(data1, c("series", "date"),
             function(x) {
                 touse <- setdiff(names(x), c("bond", "series", "date"))
                 wgt <- x[["wgt"]]
                 fun <- function(y) weighted.mean(y, wgt)
                 colwise(fun, touse)(x)
             })

write.csv(data2, file = outfile, row.names = FALSE)
