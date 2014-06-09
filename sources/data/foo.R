foo <- read.csv("merchants_magazine_us_paper.csv", stringsAsFactors = FALSE)
bar <- read.csv("merchants_magazine_series.csv", stringsAsFactors = FALSE)
baz <- merge(foo, bar, by = "series", all.x = TRUE)
baz[["series"]] <- baz[["longname"]]
baz[["longname"]] <- NULL
write.csv(baz, file = "merchants_magazine_us_paper.csv", na = "", row.names = FALSE)

