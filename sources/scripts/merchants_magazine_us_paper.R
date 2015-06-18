#' Merchants' Magazine
#'
#' Adds price in gold. Gold prices come from ``greenbacks_fill``.
source("R/.init.R")

args <- commandArgs(TRUE)
src <- args[1]
dst <- args[2]

greenbacks <-
    (mutate(read_csv("data/greenbacks_fill.csv"),
            date = as.Date(date, "%Y-%m-%d"),
            gold_rate = 100 / mean)
     %>% select(date, gold_rate))
merchants <- mutate(read_csv(src),
                     date = as.Date(date, "%Y-%m-%d"))
merchants <- merge(merchants, greenbacks, by = "date", all.x = TRUE)
# convert August demand notes from premium to price
merchants$value[merchants$series == "August demand notes"] <-
  100 + merchants$value[merchants$series == "August demand notes"]
# no conversion to gold when series is gold
merchants$gold_rate[merchants$series %in% c("Gold", "Gold, low", "Gold, high")] <- 1
merchants <-
    (mutate(merchants,
            price_currency = value,
            price_gold = price_currency / gold_rate,
            adjust_gold = fill_na(adjust_gold),
            adjust_currency = fill_na(adjust_currency),
            is_clean = fill_na(is_clean),
            registered = as.integer(grepl("Reg\\.", series)))
     %>% select(-value))
write_csv(merchants, file = dst)
