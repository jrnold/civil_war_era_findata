#' Yields on Greenbacks held until 1879-1-1
library("plyr")
args <- commandArgs(TRUE)

infile <- "data/greenbacks.csv"
infile <- args[1]
outfile <- args[2]

greenbacks <-
    mutate(read.csv(infile),
           date = as.Date(date))

MATURITY_DATE <- as.Date("1879-1-1")

greenback_yields <-
    mutate(greenbacks,
           maturity = (as.integer(difftime(MATURITY_DATE, date, units = "days"))) / 365,
           low = - log(low / 100) / maturity,
           high = - log(high / 100) / maturity,
           mean = - log(mean / 100) / maturity)

write.csv(greenback_yields, file = outfile, row.names = FALSE)
