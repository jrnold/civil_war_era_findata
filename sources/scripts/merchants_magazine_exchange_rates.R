#' Clean Merchants' Magazine data
#'
source("R/.init.R")

cities_names <- c(london = "London", paris = "Paris",
                  amsterdam = "Amsterdam", frankfort = "Frankfort",
                  hamburg = "Hamburg", berlin = "Berlin")

#' Reshapes the data to have units date, city.
#' Additionally fixes a few issues with the data
#' - indicate when the data are quoted in currency vs. gold dollars
#' - before 1861-12-1, London was listed as just the premium, e.g. 6 vs. 106.
merchants <- read_csv("sources/data/Merchants_FX_data.csv") %>%
  select(-strdate) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
  gather(city, rate, -date, -url, -issue) %>%
  mutate(city = as.character(city)) %>%
  filter(rate != "", !grepl("NA *NA", rate)) %>%
  mutate(rate = str_trim(rate)) %>%
  separate(rate, c("rate_low", "rate_high"), sep = " +") %>%
  mutate(rate_low = as.numeric(rate_low),
         rate_high = as.numeric(ifelse(rate_high == "NA",
                                       rate_low, rate_high))) %>%
  mutate(city = cities_names[city]) %>%
  mutate(rate_low = ifelse(city == "London" & date < as.Date("1861-12-01"),
                           rate_low + 100, rate_low),
         rate_high = ifelse(city == "London" & date < as.Date("1861-12-01"),
                            rate_high + 100, rate_high),
         currency = (date >= as.Date("1862-1-1")) &
           (date < as.Date("1864-10-22")))

write_csv(merchants, file = "data/merchants_exchange_rates.csv")
