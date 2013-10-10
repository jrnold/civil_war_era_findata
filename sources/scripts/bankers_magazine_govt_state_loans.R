#' Bankers magazine
#'
#' Adds price in gold. Gold prices come from ``greenbacks_fill``.
library(plyr)
source("sources/scripts/R/misc.R")

args <- commandArgs(TRUE)
src <- args[1]
dst <- args[2]

greenbacks <- mutate(read.csv("data/greenbacks_fill.csv"),
                     date = as.Date(date, "%Y-%m-%d"),
                     gold = 100 / mean)[ , c("date", "gold")]
bankers <- mutate(read.csv(src),
                  date = as.Date(date, "%Y-%m-%d"))
bankers <- merge(bankers, greenbacks, by = "date", all.x = TRUE)
bankers$gold[is.na(bankers$gold)] <- 1
bankers <- mutate(bankers,
                  price_paper = value,
                  price_gold = price_paper / gold,
                  value = NULL, gold = NULL)

write.csv2(bankers, file = dst)
