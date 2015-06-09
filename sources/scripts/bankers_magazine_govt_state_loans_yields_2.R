source("R/.init.R")

sysargs <- commandArgs(TRUE)
infile <- sysargs[1]
infile <- "data/bankers_magazine_govt_state_loans_yields.csv"
outfile <- sysargs[2]
outfile <- "data/bankers_magazine_govt_state_loans_yields_2.csv"

data1 <- read_csv(infile)
touse <- setdiff(names(data1), c("bond", "series", "date", "wgt"))
data2 <-
  data1 %>%
  group_by(series, date) %>%
  summarise_each_(funs(weighted.mean(., wgt)), touse)
write_csv(data2, file = outfile)
