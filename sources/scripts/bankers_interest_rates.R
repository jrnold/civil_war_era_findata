#' Bankers Magazine Short Run Interest Rates
#'
bankers <- read_csv("sources/data/bankers_magazine_commercial_paper_data.csv") %>%
  gather(date, rate, - type) %>%
  mutate(date = as.Date(date, format = "%b_%d_%Y")) %>%
  filter(rate != "") %>%
  separate(rate, c("rate_low", "rate_high"), sep = " *@ *")

write_csv(bankers, file = "data/bankers_commercial_paper.csv")

