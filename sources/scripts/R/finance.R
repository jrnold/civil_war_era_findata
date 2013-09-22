library("lubridate")
library("plyr")

#' Interpolate time series using tsSmooth
ts_interpolate <- function (x, ...) {
    xhat <- tsSmooth(StructTS(x, ...))[, 1]
    ifelse(is.na(x), xhat, x)
}

#' Is it the last day of month?
last_dom <- function(x) {
    month(x) != month(x + days(1))
}

#' Is day the last day of february
last_day_of_feb <- function(x) {
    (month(x) == 2) & last_dom(x)
}

#' Number of days using 30/360 day count
#'
#' Calculates the difference in days between two dates using the US
#' 30/360 day count, i.e. treating the year as 12 months of 30 days.
#'
difftime_30_360 <- function(time2, time1) {
    d1 <- mday(time1)
    d2 <- mday(time2)
    m1 <- month(time1)
    m2 <- month(time2)
    y1 <- year(time1)
    y2 <- year(time2)
    # if D2 is the last day of February (28 in a non leap year; 29 in
    # a leap year) and D1 is the last day of February, change D2 to 30
    ldfeb1 <- last_day_of_feb(time1)
    ldfeb2 <- last_day_of_feb(time2)
    d2[ldfeb2 & ldfeb1] <- 30
    ## if D1 is the last day of February, change D1 to 30
    d1[ldfeb1] <- 30
    ## if D2 is 31 and D1 is 30 or 31, change D2 to 30
    d2[d2 == 31 & (d1 %in% c(30, 31))] <- 30
    ## if D1 is 31, change D1 to 30
    d1[d1 == 31] <- 30
    ## Calculate difference in days
    (d2 - d1) + 30 * (m2 - m1) + 360 * (y2 - y1)
}


#' Calculate Duration Yield (Yield to maturity)
ytm <- function(x, m, searchint = c(-1, 1), tol = 1e-10) {
    pvcashflows <- function(y) {
        t(x) %*% exp(-m * y)
    }
    uniroot(pvcashflows, searchint, tol = tol,  maxiter = 3000)$root
}

z_spread <- function(cashflow, maturity, discount, searchint = c(-1, 1), tol = 1e-10) {
  pvcashflows <- function(y) {
    t(cashflow) %*% exp(-maturity * (discount + y))
  }
  uniroot(pvcashflows, searchint, tol = tol,  maxiter = 3000)$root
}

macaulay_duration <- function(cashflows, maturity, yield, price) {
  sum(maturity * cashflows * exp(-yield * maturity)) / price
}

current_yield <- function(issue, price, face=100, ...) {
    current <- face * cashflows[[as.character(issue)]]$interest / price
}

prev_date <- function(x, table) {
    indic <- which(table <= x)
    if (length(indic)) {
        table[max(indic)]
    } else {
        NA
    }
}

next_date <- function(x, table) {
    indic <- which(x > table)
    if (length(indic)) {
        table[min(indic)]
    } else {
        NA
    }
}

#' Since the prices listed in the Bankers' Magazine often do not explicitly
#' state the bond issue, there are multiple issues for each price. This applies
#' weights to each issue price. They are equally weighted with some exceptions
#'
#' - Five-twenties. Use only the old issue (1862's) since they are quoted
#'   1862-2-19 to 1864-12-26.
bond_weights <-
    list(us_1868 =
         data.frame(issue = c("sixes_18680101", "sixes_18680701"),
                    wgt = c(0.590, 0.410)),
         us_1881 =
         data.frame(issue = c("sixes_18810101", "sixes_18810701"),
                    wgt = c(0.089, 0.911)),
         # until 1864-8-15
         seven_thirties_1 =
         data.frame(issue = c("seven_thirties_18640819", "seven_thirties_18641001"),
                    wgt = c(0.5, 0.5)),
         # 1864-8-15 to 1864-8-19
         seven_thirties_2 =
         data.frame(issue =
                    c("seven_thirties_18640819", "seven_thirties_18641001",
                      "seven_thirties_18670815"),
                    wgt = c(0.168, 0.168, 0.664)),
         # 1864-8-19 to 1864-10-1
         seven_thirties_3 =
         data.frame(issue =
                    c("seven_thirties_18641001", "seven_thirties_18670815"),
                    wgt = c(0.202, 0.798)),
         # 1864-10-1 to 1864-12-31
         seven_thirties_4 =
         data.frame(issue = c("seven_thirties_18670815"), wgt = c(1)),
         # Five-twenties
         five_twenties =
         data.frame(issue =
                    c("five_twenty_1862_14", "five_twenty_1862_15", "five_twenty_1862_16",
                      "five_twenty_1862_13", "five_twenty_1862_5", "five_twenty_1862_6",
                      "five_twenty_1862_7", "five_twenty_1862_8", "five_twenty_1862_9",
                      "five_twenty_1862_10", "five_twenty_1862_11", "five_twenty_1862_12",
                      "five_twenty_1862_17", "five_twenty_1862_18", "five_twenty_1862_19",
                      "five_twenty_1862_20"),
                    wgt =
                    c(0.0625, 0.0625, 0.0625, 0.0625, 0.0625, 0.0625, 0.0625, 0.0625,
                      0.0625, 0.0625, 0.0625, 0.0625, 0.0625, 0.0625, 0.0625, 0.0625)),
         ## Louisiana
         ##
         ## set of comparisons
         ##
         ## - Kentucky: 1869-72
         ## - Missouri: 1872
         ## - North Carolina: 1873
         ## - Pennsylvania: 1873
         ## - Virginia: 1885-1890
         ## - Tennessee: 1885-1892
         ## yrs <-
         ## data.frame(yr= c(1869:1872, 1872, 1873, 1885:1890, 1885:1892),
         ##            wt = c(rep(0.25, 4), 1, 1, rep(1/6, 6), rep(1/8, 8)))
         ## dput(transform(ddply(yrs, "yr", summarise, wt = sum(wt)), wt = wt / sum(wt))$wt)
         louisiana =
         data.frame(wgt =
                    c(0.05, 0.05, 0.05, 0.25, 0.2, 0.0583333333333333, 0.0583333333333333,
                      0.0583333333333333, 0.0583333333333333, 0.0583333333333333, 0.0583333333333333,
                      0.025, 0.025),
                    issue =
                    c("louisiana_six_18690701", "louisiana_six_18700701",
                      "louisiana_six_18710701", "louisiana_six_18720701",
                      "louisiana_six_18730701",
                      "louisiana_six_18850701", "louisiana_six_18860701",
                      "louisiana_six_18870701", "louisiana_six_18880701",
                      "louisiana_six_18890701", "louisiana_six_18900701",
                      "louisiana_six_18910701", "louisiana_six_18920701")),
         ## Indiana 5
         ##
         ## set of comparisons
         ##
         ## - Kentucky: 1869-72
         ## - Missouri: 1872
         ## - North Carolina: 1873
         ## - Pennsylvania: 1873
         ## - Virginia: 1885-1890
         ## - Tennessee: 1885-1892
         ## - Georgia: 1872
         ## yrs <-
         ## data.frame(yr= c(1869:1872, 1872, 1873, 1885:1890, 1885:1892, 1872),
         ##            wt = c(rep(0.25, 4), 1, 1, rep(1/6, 6), rep(1/8, 8), 1))
         ## dput(transform(ddply(yrs, "yr", summarise, wt = sum(wt)), wt = wt / sum(wt))$wt)
         indiana5 =
         data.frame(wgt =
                    c(0.0416666666666667, 0.0416666666666667, 0.0416666666666667,
                    0.375, 0.166666666666667, 0.0486111111111111, 0.0486111111111111,
                    0.0486111111111111, 0.0486111111111111, 0.0486111111111111, 0.0486111111111111,
                    0.0208333333333333, 0.0208333333333333),
                    issue =
                    c("indiana_five_18607019", "indiana_five_18700701", "indiana_five_18710701",
                      "indiana_five_18720701", "indiana_five_18730701",
                      "indiana_five_18850701", "indiana_five_18860701", "indiana_five_18870701",
                      "indiana_five_18880701", "indiana_five_18807019", "indiana_five_18070190",
                      "indiana_five_18070191", "indiana_five_18070192"))
         )

#' Calculate weights for bond issues for each price series
weight_bond_issues <- function(x) {
    k <- as.character(x$bond[1])
    if (k == "us_1868") {
        merge(x, issue_weights[[k]])
    } else if (k == "us_1881") {
        merge(x, issue_weights[[k]])
    } else if (k == "seven_thirties") {
        date <- x$date[1]
        if (date < as.Date("1864-8-15")) {
            merge(x, issue_weights[["seven_thirties_1"]])
        } else if (date >= as.Date("1864-8-15") & date < as.Date("1864-8-19")) {
            merge(x, issue_weights[["seven_thirties_2"]])
        } else if (date >= as.Date("1864-8-19") & date < as.Date("1864-10-1")) {
            merge(x, issue_weights[["seven_thirties_3"]])
        } else if (date >= as.Date("1864-10-1")) {
            merge(x, issue_weights[["seven_thirties_4"]])
        }
    } else if (k == "five_twenty_coup") {
        merge(x, issue_weights[["five_twenties"]])
    } else if (k == "five_twenty_reg") {
        merge(x, issue_weights[["five_twenties"]])
    } else if (k == "louisiana") {
        merge(x, issue_weights[["louisiana"]])
    } else if (k == "indiana5") {
        merge(x, issue_weights[["indiana5"]])
    } else {
        x$wgt <- 1 / nrow(x)
        x
    }
}

#' Calculate accrued for a bond
accrued_interest <- function(issue, cashflows, date, face=100, ...) {
    datelist <- as.Date(sort(c(cashflows$issued, cashflows$cf$date)),
                        as.Date("1970-1-1"))
    lastcoupon <- prev_date(as.Date(date), datelist)
    if (!is.na(lastcoupon)) {
        n <- length(cashflows$periods)
        factor <- difftime_30_360(date, lastcoupon) / 360
        (factor * cashflows$interest * face)
    } else {
        0
    }
}

#' Calculate accrued for a bond
calc_gold_yield <- function(issue, date, gold, searchint=c(-1, 1), ...) {
  price <- gold
  x <- cashflows[[as.character(issue)]]$cf
  x <- x[x$date > date, , drop=FALSE]
  if (nrow(x)) {
    ## Accrued interest
    accrued <- accrued_interest(issue, date)
    clean_price <- price - accrued
    
    m <- difftime_30_360(x$date, date) / 360
    res <- bond_yields(c(-price, x$cashflow), c(0, m), searchint = searchint)
    if (is(res, "try-error")) {
      ytm <- NA
    } else {
      ytm <- res
    }
    ## Macaulay duration
    duration <- macaulay_duration(x$cashflow, m, ytm, clean_sprice)
    ## Current yield
    current <- current_yield(issue, clean_price)
    data.frame(yield = ytm, duration = duration,
               maturity = max(m), current = current, accrued = accrued,
               clean_price = clean_price)
  } else {
    data.frame(yield = NA, duration=NA, maturity = NA, current=NA,
               accrued = 0, clean_price = price)
  }
}

