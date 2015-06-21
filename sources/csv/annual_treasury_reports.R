#' ---------------------------
#' Treasury Report
#' ---------------------------
#'
source("R/init.R")

dst <- commandArgs(TRUE)[1]

.data <- list()

#' 1860
#' =========

#' Fiscal year 1860
.data[["1860-12-1"]][["1860"]] <-
    list(
        balance = 4339275.54,
        receipts = (15947670 + 470244.62 + 379650.61
                   + 10785849.93 + 445535.36 + 149392.76
                   + 14962783.68 + 505591.83 + 245447.36
                   + 11491207.64 + 357185.90 + 236273.58),
        loans =  (3611300 + 21000
                  + 4064500 + 60000
                  + 5588200 + 1110000
                  + 6131200),
        expenditures =  77462102.72,
        debt_principal = 14436313.38
        )

#' Fiscal year 1861
.data[["1860-12-1"]][["1861"]] <-
    list(
         balance =  3629206.71,
         receipts =  59719790.04,
         expenditures =  84348996.75,
         debt_principal =  21115560.47,
         loans =  21000000.00
         )

#' Fiscal year 1862
.data[["1860-12-1"]][["1862"]] <-
    list(
         balance = 245891.58,
         receipts = 65250000.00,
         expenditures = 68363726.11,
         debt_principal = NA,
         loans = 3867834.53
         )

#' 1861 (Jul)
#' ==========

#' Fiscal year 1861
.data[["1861-7-4"]][["1861"]] <-
    list(
        balance = 3629206.71,
        receipts = 41279604.15,
        expenditures = 84577258.60,
        debt_principal = 18219207.27,
        loans = 42064082.95
        )

#' Fiscal year 1862
.data[["1861-7-4"]][["1862"]] <-
    list(
        balance = 2355635.21,
        receipts = 80000000.00,
        expenditures = 318519581.87,
        debt_principal = 12639861.64,
        loans = 238519581.87
        )

#' 1861 (Dec)
#' ===============

#' 1861
.data[["1861-12-9"]][["1861"]] <-
    list(
        balance = 3629206.71,
        receipts = 39582125.64 + 870658.54 + 892199.64,
        loans = 41861709.74,
        expenditures = 84578834.47,
        debt_principal = 78807.27 + 1000 + 4000173.76
        )

#' 1862
.data[["1861-12-9"]][["1862"]] <-
    list(
        balance = 2257065.80,
        receipts = 36809731.24 - 2257065.80 + 20000000,
        expenditures = 543406422.06,
        debt_principal = 22627651.50 + 112092.59 + 22870398.50,
        loans = 197242588.14 + 75449675.00 + 213904417.68
        )

#' 1863
.data[["1861-12-9"]][["1863"]] <-
    list(
        balance = 0,
        receipts = 95800000.00,
        debt_principal = 2883364.11,
        expenditures = 475331245.51,
        loans = 379531245.51
        )

#' 1862
#' =============
.data[["1862-12-4"]][["1862"]] <-
    list(
        balance = 2257065.80,
        receipts = 51935720.76,
        loans = 529692460.50,
        expenditures = 570841700.25,
        debt_principal = 96096922.09
        )

.data[["1862-12-4"]][["1863"]] <-
    list(
        balance = 13043546.81,
        receipts = 167451798.79,
        loans = 608063432.02,
        expenditures = 788558777.62,
        debt_principal = 95212456.14
        )

.data[["1862-12-4"]][["1864"]] <-
    list(
        balance = 0,
        receipts = 223025000.00,
        loans = 622388183.56,
        expenditures = 845413183.56,
        debt_principal = 19384804.16
        )

#' 1863
#' =============
.data[["1863-12-10"]][["1863"]] <-
    list(
        balance = 13043546.81,
        receipts = 111399766.48,
        loans = 776682361.57,
        expenditures = 895796630.65,
        debt_principal = 181086635.07
        )

.data[["1863-12-10"]][["1864"]] <-
    list(
        balance = 5329044.21,
        receipts = 156239456.14,
        loans = 129842432.11 + 458321027.96,
        expenditures = 749731960.42,
        debt_principal = 0
        )

.data[["1863-12-10"]][["1865"]] <-
        list(
            balance = 5836539.93,
            receipts = 206836539.93 - 5836539.93,
            loans = 544978548.93,
            expenditures = 751815088.86,
            debt_principal = 0
            )

#' 1864
#' =================
.data[["1864-12-6"]][["1864"]] <-
    list(
        balance = 5329044.21,
        receipts = 260632717.44,
        loans = 618114884.92,
        expenditures = 865234087.86,
        debt_principal = 0
        )

.data[["1864-12-6"]][["1865"]] <-
    list(
        receipts = 419512389.02 + 50000000,
        balance = 18842558.71,
        loans = 570727508.11,
        expenditures = 1409082455.84 - 350000000,
        debt_principal = 88353320.09
        )

.data[["1864-12-6"]][["1866"]] <-
    list(
        balance = 0,
        receipts = 396000000,
        loans = 469621005.17,
        expenditures = 818256005.17 + 47365000,
        debt_principal = 47365000
        )

#' Data Frame
#' ============
DATA <- mutate(plyr::ldply(names(.data),
                           function(i) {
                               x <- plyr::ldply(.data[[i]], data.frame)
                               x[["report"]] <- as.Date(i, "%Y-%m-%d")
                               x
                           }),
               resources = balance + receipts + loans,
               diff = resources - expenditures)

write_csv(DATA, file = dst)
