#' Merchants' Magazine Short Run Interest Rate
#' 

FILE <- "sources/data/Merchants_Commercial_Paper_Data.csv"

merchants <- read_csv(FILE) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
  gather(type, rate, - date, - issue, - url) %>%
  filter(rate != "") %>%
  separate(rate, c("rate_low", "rate_high"), sep = " *@ *", convert = TRUE)