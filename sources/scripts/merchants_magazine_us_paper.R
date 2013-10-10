#' Merchants' Magazine
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
merchants <- mutate(read.csv(src),
                  date = as.Date(date, "%Y-%m-%d"))
merchants <- merge(merchants, greenbacks, by = "date", all.x = TRUE)
# convert August demand notes from premium to price
merchants$value[merchants$series == "aug_demand"] <-
  100 + merchants$value[merchants$series == "aug_demand"] 
merchants$gold[merchants$series %in% c("gold_low", "gold_high")] <- 1
merchants <- mutate(merchants,
                  price_paper = value,
                  price_gold = price_paper / gold,
                  value = NULL, gold = NULL)
write.csv2(merchants, file = dst)
