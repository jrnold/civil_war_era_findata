#!/usr/bin/env Rscript
library("stringr")

infile <- commandArgs(TRUE)[1]

extract_depends <- function(src) {
  PATTERN <- "^### depends: (.*)$"
  str_c(Filter(Negate(is.na), str_match(readLines(src), PATTERN)[ , 2]),
        collapse = " ")
}

generate_outfile <- function(src, newdir, newext) {
  file.path(newdir, str_c(tools::file_path_sans_ext(basename(src)), newext))
}

generate_mk_depends <- function(src) {
  outfile <- switch(
    dirname(src),
    "sources/csv" = generate_outfile(src, "data", ".csv"),
    "sources/json" = generate_outfile(src, "data", ".json")
  )
  str_c(outfile, " : ", src, " ", extract_depends(src))
}

write_mkfile <- function(src) {
  outfile <- str_c(tools::file_path_sans_ext(src), ".mk")
  cat(generate_mk_depends(src), "\n", file = outfile)
}

write_mkfile(infile)
