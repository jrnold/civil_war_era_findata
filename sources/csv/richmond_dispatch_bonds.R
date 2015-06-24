#' Confederate Interest Rates
#'
source("R/init.R")

### depends: sources/data/richmond_mkt.csv
src <- "sources/data/richmond_mkt.csv"
### depends: data/graybacks_fill.csv
graybacks_fill <- "data/graybacks_fill.csv"

richmond <- read_csv(src) %>%
  mutate(date = as.Date(date),
         newspaper_date = as.Date(date),
         past_due = as.integer(grepl("past\\s*due", description)))


