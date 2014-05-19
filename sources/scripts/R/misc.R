write.csv2 <- function(...) {
    write.csv(..., na = "", row.names=FALSE)
}

fill_na <- function(x, fill=0) {
    x[is.na(x)] <- fill
    x
}
