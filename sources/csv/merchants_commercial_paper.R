#' Merchants' Magazine Short Run Interest Rate
#'

### depends: sources/data/Merchants_Commercial_Paper_Data.csv
src <- "sources/data/Merchants_Commercial_Paper_Data.csv"
dst <- commandArgs(TRUE)[1]

merchants <- read_csv(src) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
  gather(type, rate, - date, - issue, - url) %>%
  filter(rate != "") %>%
  separate(rate, c("rate_low", "rate_high"), sep = " *@ *", convert = TRUE)

write_csv(merchants, file = dst)
