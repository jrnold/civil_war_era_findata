#' Clean the Bankers' Magazine FX Rate Data
#'
#'

cities_names <- c(london_60_days = "London",
                  london_mercantile = "London",
                  london_lading = "London",
                  paris = "Paris",
                  amsterdam = "Amsterdam",
                  bremen = "Bremen",
                  hamburg = "Hamburg",
                  frankfort = "Frankfort")

bills_type <- c(london_60_days = "bankers' bills",
                                london_mercantile = "mercantile bills",
                                london_lading = "with bills of lading",
                                paris = "bankers' bills",
                                amsterdam = "per guilder",
                                bremen = "per rix dollar",
                                hamburg = "per marc banco",
                                frankfort = "per florin")


bankers <- read_csv("sources/data/bankers_magazine_fx_data.csv") %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
  gather(city, rate, - date, - issue, - url) %>%
  filter(rate != "") %>%
  separate(rate, c("rate_low", "rate_high"), sep = "@", extra = "drop") %>%
  mutate(rate_low = as.numeric(ifelse(rate_low == ".", rate_high, rate_low)),
         rate_high = as.numeric(ifelse(rate_high == ".", rate_low, rate_high))) %>%
  mutate(type = bills_type[city],
         city = cities_names[city])



