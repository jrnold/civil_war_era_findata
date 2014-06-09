foo <- read.csv("bankers_magazine_govt_state_loans.csv", stringsAsFactors = FALSE)
bar <- read.csv("bankers_magazine_series.csv", stringsAsFactors = FALSE)
baz <- merge(foo, bar, by = "series", all.x = TRUE)
baz[["series"]] <- baz[["longname"]]
baz[["longname"]] <- 

