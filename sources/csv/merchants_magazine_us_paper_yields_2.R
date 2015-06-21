source("R/init.R")

### depends: data/merchants_magazine_us_paper_yields.csv
infile <- "data/merchants_magazine_us_paper_yields.csv"
outfile <- commandArgs(TRUE)[1]

data2 <-
  read_csv(infile) %>%
  group_by(series, date) %>%
  summarise_each(funs(weighted.mean(., wgt)),
                  - bond, - series, - date, - wgt)
write_csv(data2, file = outfile)
