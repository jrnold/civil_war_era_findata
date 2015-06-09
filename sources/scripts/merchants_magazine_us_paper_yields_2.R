source("R/.init.R")

sysargs <- commandArgs(TRUE)
infile <- sysargs[1]
infile <- "data/merchants_magazine_us_paper_yields.csv"
outfile <- sysargs[2]
outfile <- "data/merchants_magazine_us_paper_yields_2.csv"

data2 <-
  read_csv(infile) %>%
  group_by(series, date) %>%
  summarise_each(funs(weighted.mean(., wgt)),
                  - bond, - series, - date, - wgt)
write_csv(data2, file = outfile)
