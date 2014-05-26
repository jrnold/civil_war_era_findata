library("lubridate")
library("dplyr")

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
#'
#' @param price dirty price
#' @param x vector of cashflow amounts
#' @param m vector of cashflow times
#' @param searchint search interval for the yield
#' @param ... Passed to \code{\link{uniroot}}
yield_to_maturity <- function(price, x, m, interval = c(0, 1), ...) {
    pvcashflows <- function(y) {
        price - t(x) %*% exp(-m * y)
    }
    ytm <- uniroot(pvcashflows, interval = interval, ...)$root
    attr(ytm, "duration") <-  t(x * m) %*% exp(-m * ytm) / price
    attr(ytm, "convexity") <- t(x * m^2) %*% exp(-m * ytm) / price
    attr(ytm, "maturity") <- max(m)
    ytm
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

#' @param gold_rate gold exchange rate: dollars per gold_dollar
gold_cashflows <- function(cashflows, gold_rate) {
    mutate(cashflows,
           amount = ifelse(specie, amount, amount / gold_rate))
}

normalize_cashflows <- function(effectiveDate, cashflows) {
    (mutate(cashflows,
            maturity = as.integer(difftime(date, effectiveDate, units = "days")) / 365)
     %>% filter(maturity > 0)
     %>% select(maturity, amount)
     )
}

next_coupon <- function(date, cashflows) {
    cf <- filter(cashflows, interest)
    if (date >= max(cf$date)) {
        list(date = NA, coupon = 0)
    } else {
        dt <- min(cf$date[cf$date > date])
        list(date = dt, coupon = cf[cf$date == dt, "amount"])
    }
}

prev_coupon <- function(date, cashflows) {
    cf <- filter(cashflows, interest)
    if (date < min(cf$date)) {
        list(date = NA, coupon = 0)
    } else {
        dt <- max(cf$date[cf$date <= date])
        list(date = dt, coupon = cf[cf$date == dt, "amount"])
    }
}

coupon_factor <- function(date, cashflows, issue_date = NULL) {
    datelist <- sort(cashflows$date)
    if (! is.null(issue_date) && ! is.na(issue_date)) {
        datelist <- c(issue_date, datelist)
    }
    if (date < min(datelist) || date >= max(datelist)) {
        ret <- 0
    } else {
        mindate <- max(datelist[date >= datelist])
        maxdate <- min(datelist[date < datelist])
        ret <- (as.integer(difftime(date, mindate, units = "days"))
                / as.integer(difftime(maxdate, mindate, units = "days")))
    }
    ret
}

accrued_interest <- function(date, cashflows, issue_date = NULL) {
    (next_coupon(date, cashflows)$coupon
     * coupon_factor(date, cashflows, issue_date))
 }

yield_to_maturity2 <- function(price, date, cashflows) {
    cf <- normalize_cashflows(date, cashflows)
    yield_to_maturity(price, cf$amount, cf$maturity)
}

make_bond_table <- function(bonds) {
    data.frame(bond = bonds, wgt = 1 / length(bonds))
}

make_bond_table_regex <- function(pattern, metadata) {
    bonds <- grep(pattern, names(metadata), value = TRUE)
    make_bond_table(bonds)
}

make_bond_table_dist <- function(pattern, .list, prior_yrs = NULL, prior_n = 1) {
    .data <- plyr::ldply(.list, function(x) data.frame(year = x, wgt = 1 / length(x)))
    if (is.null(prior_yrs)) {
        minyr <- min(.data$year)
        maxyr <- max(.data$year)
        prior_yrs <- seq(minyr, maxyr, by = 1)
    } 
    .prior <- data.frame(year = prior_yrs, wgt = 1 / length(prior_yrs) * prior_n)
    (group_by(plyr::rbind.fill(.data, .prior), "year")
     %>% summarise(wgt = sum(wgt))
     %>% mutate(wgt = wgt / sum(wgt),
                bond = sprintf(pattern, year))
     %>% select(bond, wgt))
}
