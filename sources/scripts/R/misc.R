library("jsonlite")
library("dplyr")
library("magrittr")

get_bond_metadata <- function(file) {
    lapply(fromJSON(file),
           function(x) {
               x[["cashflows"]] %<>% mutate(date = as.Date(date))
               x[["maturity_date"]] %<>% as.Date()
               x[["issue_date"]] %<>% as.Date()
               x
           })
}

read_csv <- function(...) {
    read.csv(..., stringsAsFactors = FALSE)
}
write_csv <- function(...) {
    write.csv(..., na = "", row.names = FALSE)
}

fill_na <- function(x, fill=0) {
    x[is.na(x)] <- fill
    x
}

exp_mean_log <- function(x, y) {
    exp(0.5 * (log(x) + log(y)))
}

difftime_years <- function(x, y) {
    as.integer(difftime(x, y, units = "days")) / 365
}
