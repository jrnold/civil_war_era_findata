#' Clean the Bankers' Magazine FX Rate Data
#' 
bankers <- read_csv("sources/data/bankers_magazine_fx_data.csv") %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
  gather(city, rate, - date, - issue, - url) %>%
  filter(rate != "") %>%
  separate(rate, c("rate_low", "rate_high"), sep = "@", extra = "drop", convert = TRUE)


