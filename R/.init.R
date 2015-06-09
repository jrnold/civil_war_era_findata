library("jsonlite")
library("dplyr")
library("magrittr")
library("lubridate")

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
yield_to_maturity <- function(price, x, m, interval = c(-1.5, 1.5), ...) {
    pvcashflows <- function(y) {
        price - t(x) %*% exp(-m * y)
    }
    ytm <- try(uniroot(pvcashflows, interval = interval, ...)$root,
               silent = TRUE)
    if (!inherits(ytm, "try-error")) {
      attr(ytm, "duration") <-  as.numeric(t(x * m) %*% exp(-m * ytm) / price)
      attr(ytm, "convexity") <- as.numeric(t(x * m^2) %*% exp(-m * ytm) / price)
      attr(ytm, "maturity") <- as.numeric(max(m))
    } else {
      ytm <- NA_real_
      attr(ytm, "duration") <-  NA_real_
      attr(ytm, "convexity") <- NA_real_
      attr(ytm, "maturity") <- NA_real_
    }
    ytm
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

#' Gold rate = dollars per gold dollar
gold_redemp_time <- function(gold_rate, r = 0.05) {
  - log(1 / gold_rate) / r
}

gold_redemp_date <- function(current_date, gold_rate, r = 0.05) {
  current_date + gold_redemp_time(gold_rate, r) * 365
}

future_gold_rates <- function(dates, current_date, gold_rate, r = 0.05,
                               float_date = as.Date("1862-1-1")) {
  redemp_date <- gold_redemp_date(current_date, gold_rate, r)
  times <- difftime_years(dates, current_date)
  data.frame(date = dates,
                    gold_rate = pmax(1, gold_rate * exp(- r * times))) %>%
    mutate(gold_rate = ifelse(date < as.Date(float_date), 1, gold_rate))
}

#' @param gold_rate gold exchange rate: dollars per gold_dollar
#' @param redemp Gold redemption rate
#' @param r Interest rate
gold_cashflows_redemp <- function(cashflows, current_date, gold_rate, r) {
  future_rates <- future_gold_rates(as.Date(cashflows$date),
                                    current_date, gold_rate, r)
  mutate(cashflows,
         amount = ifelse(specie, amount * future_rates$gold_rate, amount))
}

normalize_cashflows <- function(effectiveDate, cashflows) {
    (mutate(cashflows,
            maturity = difftime_years(date, effectiveDate))
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
    datelist <- sort(filter(cashflows, interest)$date)
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
    .data <- plyr::ldply(.list, function(x) {
      data.frame(year = x, wgt = 1 / length(x))
      })
    if (is.null(prior_yrs)) {
        minyr <- min(.data$year)
        maxyr <- max(.data$year)
        prior_yrs <- seq(minyr, maxyr, by = 1)
    }
    .prior <- data.frame(year = prior_yrs, wgt = 1 / length(prior_yrs) * prior_n)
    (group_by(plyr::rbind.fill(.data, .prior), year)
     %>% summarise(wgt = sum(wgt))
     %>% mutate(wgt = wgt / sum(wgt),
                bond = sprintf(pattern, year))
     %>% select(bond, wgt))
}

calc_current_yield <- function(date, clean_price, cashflows, n) {
    next_coupon(date, cashflows)$coupon * n / clean_price
}

make_yields_etc <-
    function(date, bond, gold_rate, price_gold, adjust_gold, adjust_currency,
             is_clean, metadata)
{
    issue_date <- metadata[["issue_date"]]
    # Cashflows keeping gold to currency = 1
    cashflows <- metadata$cashflows

    # Price in gold dollars
    accrued <- accrued_interest(date, cashflows, issue_date)
    price <- price_gold + adjust_gold + adjust_currency / gold_rate
    if (! is_clean) {
        price_clean <- price - accrued
    } else {
        price_clean <- price
        price <- price_clean + accrued
    }
    price_currency <- price * gold_rate
    price_clean_currency <- price_clean * gold_rate


    # yields using currency
    yields_currency <- yield_to_maturity2(price_currency, date,
                                          cashflows)
    if (gold_rate != 1) {
      # yields using gold current price
      yields_gold <-
        gold_cashflows(cashflows, gold_rate) %>%
        yield_to_maturity2(price, date, .)
      # yields using currency with implied gold redemption
      yields_currency3 <-
        gold_cashflows_redemp(cashflows, date, gold_rate,
                              r = 0.04) %>%
        yield_to_maturity2(price_currency, date, .)
      yields_currency4 <-
        gold_cashflows_redemp(cashflows, date, gold_rate,
                              r = 0.05) %>%
        yield_to_maturity2(price_currency, date, .)
      # yields using currency with implied gold redemption
      yields_currency5 <-
        gold_cashflows_redemp(cashflows, date, gold_rate,
                              r = 0.06) %>%
        yield_to_maturity2(price_currency, date, .)
    } else {
      yields_gold <- yields_currency
      yields_currency3 <- yields_currency
      yields_currency4 <- yields_currency
      yields_currency5 <- yields_currency
    }


    # yields uing gold
    if ("periods" %in% names(metadata)
        && length(metadata$periods)
        && ! is.na(metadata$periods)) {
        current_yield <-
          gold_cashflows(cashflows, gold_rate) %>%
          calc_current_yield(date, price_clean, .,
                             length(metadata$periods))
        current_yield_currency <-
          calc_current_yield(date, price_clean_currency, cashflows,
                             length(metadata$periods))
    } else {
        current_yield <- current_yield_currency <- NA
    }
    data.frame(price = price,
               price_clean = price_clean,
               price_orig = price_gold,
               gold_rate = gold_rate,
               accrued_interest = accrued,

               current_yield1 = current_yield,
               current_yield2 = current_yield_currency,

               ytm1 = as.numeric(yields_currency),
               duration1 = attr(yields_currency, "duration"),
               convexity1 = attr(yields_currency, "convexity"),
               maturity1 = attr(yields_currency, "maturity"),

               ytm2 = as.numeric(yields_gold),
               duration2 = attr(yields_gold, "duration"),
               convexity2 = attr(yields_gold, "convexity"),
               maturity2 = attr(yields_gold, "maturity"),

               ytm3 = as.numeric(yields_currency3),
               duration3 = attr(yields_currency3, "duration"),
               convexity3 = attr(yields_currency3, "convexity"),
               maturity3 = attr(yields_currency3, "maturity"),

               ytm4 = as.numeric(yields_currency4),
               duration4 = attr(yields_currency4, "duration"),
               convexity4 = attr(yields_currency4, "convexity"),
               maturity4 = attr(yields_currency4, "maturity"),

               ytm5 = as.numeric(yields_currency5),
               duration5 = attr(yields_currency5, "duration"),
               convexity5 = attr(yields_currency5, "convexity"),
               maturity5 = attr(yields_currency5, "maturity")
               )
}
