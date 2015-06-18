#' Clean Merchants' Magazine data
#' 
source("R/.init.R")

merchants <- read_csv("sources/data/Merchants_FX_data.csv") %>%
  select(- X, - strdate) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
  gather(city, rate, - date, - url, - issue) %>%
  filter(rate != "") %>%
  separate(rate, c("rate_low", "rate_high"), sep = " +", extra = "drop", convert = TRUE)