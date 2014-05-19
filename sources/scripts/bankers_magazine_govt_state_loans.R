#' Bankers magazine
#'
#' Adds price in gold. Gold prices come from ``greenbacks_fill``.
library("dplyr")
source("sources/scripts/R/misc.R")

args <- commandArgs(TRUE)
src <- args[1]
dst <- args[2]

greenbacks <- (mutate(read.csv("data/greenbacks_fill.csv"),
                      date = as.Date(date, "%Y-%m-%d"),
                      gold_rate = 100 / mean)
               %.% select(date, gold_rate))
bankers <- mutate(read.csv(src),
                  date = as.Date(date, "%Y-%m-%d"))
bankers <- merge(bankers, greenbacks, by = "date", all.x = TRUE)
bankers$gold_rate[bankers$date < as.Date("1862-1-1")] <- 1
bankers <-
    (mutate(bankers,
           price_paper = value,
           price_gold = price_paper / gold_rate,
           adjust_gold = fill_na(adjust_gold),
           adjust_paper = fill_na(adjust_paper))
     %.% select(-value))
write.csv2(bankers, file = dst)
