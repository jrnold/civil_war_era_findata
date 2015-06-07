source(".init.R")

sysargs <- commandArgs(TRUE)
infile <- sysargs[1]
outfile <- sysargs[2]

data1 <- read_csv(infile)
touse <- setdiff(names(data1), c("bond", "series", "date", "wgt"))
data2 <- group_by(series, date) %>%
    summarise_each(funs(weighted.mean(., wgt)))
write_csv(data2, file = outfile)
