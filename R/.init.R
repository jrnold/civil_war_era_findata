library("jsonlite")
library("dplyr")
library("magrittr")
library("lubridate")

#' Load the bond metadata json file while converting fields
#' to correct types.
get_bond_metadata <- function(file) {
    lapply(fromJSON(file),
           function(x) {
               x[["cashflows"]] %<>% mutate(date = as.Date(date))
               x[["maturity_date"]] %<>% as.Date()
               x[["issue_date"]] %<>% as.Date()
               x
           })
}

#' Read csv files with better defaults
read_csv <- function(...) {
    read.csv(..., stringsAsFactors = FALSE)
}

#' Write csv files with better defaults
write_csv <- function(...) {
    write.csv(..., na = "", row.names = FALSE)
}

#' Fill in missing values
fill_na <- function(x, fill=0) {
    x[is.na(x)] <- fill
    x
}

#' Exponent of the log mean
exp_mean_log <- function(x, y) {
    exp(0.5 * (log(x) + log(y)))
}

#' Difference in years between two dates.
difftime_years <- function(x, y) {
    as.integer(difftime(x, y, units = "days")) / 365
}

#' Interpolate time series using tsSmooth
ts_interpolate <- function (x, ...) {
    xhat <- tsSmooth(StructTS(x, ...))[, 1]
    ifelse(is.na(x), xhat, x)
}

#' Is a date the last day of month?
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
      list(yield = as.numeric(ytm),
           duration = as.numeric(t(x * m) %*% exp(-m * ytm) / price),
           convexity = as.numeric(t(x * m^2) %*% exp(-m * ytm) / price),
           maturity = as.numeric(max(m)))
    } else {
      list(yield = NA_real_,
           duration = NA_real_,
           convexity = NA_real_,
           maturity = as.numeric(max(m)))
    }
}

yield_to_maturity2 <- function(price, date, cashflows) {
    cf <- normalize_cashflows(date, cashflows)
    yield_to_maturity(price, cf$amount, cf$maturity)
}



#' Find the next date less than or equal to `x` in a `table` of dates.
prev_date <- function(x, table) {
    indic <- which(table <= x)
    if (length(indic)) {
        table[max(indic)]
    } else {
        NA
    }
}

#' Find the previous date to `x` in `table` of dates.
next_date <- function(x, table) {
    indic <- which(x > table)
    if (length(indic)) {
        table[min(indic)]
    } else {
        NA
    }
}

#' Convert dollar cashflows for a bond into gold dollars using
#' `gold_rate` (dollars per gold dollar)
gold_cashflows <- function(cashflows, gold_rate) {
    mutate(cashflows,
           amount = ifelse(specie, amount, amount / gold_rate))
}


#' Convert cashflows to gold dollars using (actual) future rates
#'
#' `gold_rate` should be a dataset with columns `date` and `gold_rate`.
#'
currency_cashflows_actual <- function(cashflows, gold_rate) {
  left_join(cashflows, gold_rate, by = "date") %>%
     mutate(gold_rate = fill_na(gold_rate, 1)) %>%
     mutate(amount = ifelse(!specie, amount, amount * gold_rate))
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
  data_frame(date = dates,
                    gold_rate = pmax(1, gold_rate * exp(- r * times))) %>%
    mutate(gold_rate = ifelse(date < as.Date(float_date), 1, gold_rate))
}

future_gold_rates_t <- function(dates, current_date, gold_rate, redemp_date,
                                float_date = as.Date("1862-1-1")) {
 r <- - log(1 / gold_rate) / difftime_years(redemp_date, current_date)
 times <- difftime_years(dates, current_date)
  data_frame(date = dates,
             gold_rate = pmax(1, gold_rate * exp(- r * times))) %>%
    mutate(gold_rate = ifelse(date < as.Date(float_date), 1, gold_rate))
}

gold_cashflows_redemp <- function(cashflows, current_date, gold_rate, r) {
  future_rates <- future_gold_rates(as.Date(cashflows$date),
                                    current_date, gold_rate, r)
  mutate(cashflows,
         amount = ifelse(specie, amount * future_rates$gold_rate, amount))
}

gold_cashflows_redemp_t <- function(cashflows, current_date, gold_rate,
                                    redemp_date) {
  future_rates <- future_gold_rates_t(as.Date(cashflows$date),
                                      current_date, gold_rate, redemp_date)
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

make_bond_table <- function(bonds) {
    data_frame(bond = bonds, wgt = 1 / length(bonds))
}

make_bond_table_regex <- function(pattern, metadata) {
    bonds <- grep(pattern, names(metadata), value = TRUE)
    make_bond_table(bonds)
}

make_bond_table_dist <- function(pattern, .list, prior_yrs = NULL, prior_n = 1) {
    .data <- plyr::ldply(.list, function(x) {
      data_frame(year = x, wgt = 1 / length(x))
      })
    if (is.null(prior_yrs)) {
        minyr <- min(.data$year)
        maxyr <- max(.data$year)
        prior_yrs <- seq(minyr, maxyr, by = 1)
    }
    .prior <- data_frame(year = prior_yrs, wgt = 1 / length(prior_yrs) * prior_n)
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
             is_clean, metadata, gold_rates_actual)
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
      # yields using actual redemption time
      yields_currency6 <-
        gold_cashflows_redemp_t(cashflows, date, gold_rate,
                                redemp_date = as.Date("1879-1-1")) %>%
        yield_to_maturity2(price_currency, date, .)

    } else {
      yields_gold <- yields_currency
      yields_currency3 <- yields_currency
      yields_currency4 <- yields_currency
      yields_currency5 <- yields_currency
      yields_currency6 <- yields_currency
    }

    yields_actual <-
        currency_cashflows_actual(cashflows, gold_rates_actual) %>%
        yield_to_maturity2(price_currency, date, .)

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
    data_frame(price = price,
               price_clean = price_clean,
               price_orig = price_gold,
               gold_rate = gold_rate,
               accrued_interest = accrued,

               current_yield_currency = current_yield,
               current_yield_gold = current_yield_currency,

               ytm_currency = yields_currency$yield,
               duration_currency = yields_currency$duration,
               convexity_currency = yields_currency$convexity,
               maturity = yields_currency$maturity,

               ytm_gold = yields_gold$yield,
               duration_gold = yields_gold$duration,
               convexity_gold = yields_gold$convexity,

               ytm_gold4pct = yields_currency3$yield,
               duration_gold4pct = yields_currency3$duration,
               convexity_gold4pct = yields_currency3$convexity,

               ytm_gold5pct = yields_currency4$yield,
               duration_gold5pct = yields_currency4$duration,
               convexity_gold5pct = yields_currency4$convexity,

               ytm_gold6pct = yields_currency5$yield,
               duration_gold6pct = yields_currency5$duration,
               convexity_gold6pct = yields_currency5$convexity,

               ytm_gold1879 = yields_currency6$yield,
               duration_gold1879 = yields_currency6$duration,
               convexity_gold1879 = yields_currency6$convexity,

               ytm_actual = yields_actual$yield,
               duration_actual = yields_actual$duration,
               convexity_actual = yields_actual$convexity

               )
}

#' Time to payoff a loan with fixed payments
#'
#' @param r interest rate
#' @param amount Amount of the loan
#' @param payment Payment amount per period
#' @return Number of payments to payoff a loan.
#' 
timetopayoff <- function(r, amount, payment, periods = 1) {
    i <- r / periods
    - log(1 - i * amount / payment) / log(1 + i) * periods
}

