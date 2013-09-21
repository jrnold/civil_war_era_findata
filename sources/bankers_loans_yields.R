library(RJSONIO)
source("finance.R")

con <- file("../data/bond_metadata.json")
bond_metadata <- fromJSON(con)
close(con)

bankers_loans <-
    mutate(read.csv("../data/bankers_loans.csv"),
           date = as.Date(date, "%Y-%m-%d"))[ , c("series", "date", "value")]
           
