#' # Bond Metadata
#'
source("R/init.R")

#'
#' This generates metadata about each bond issue.
#' This is not a complete listing of government and state bond issues during this period (and in fact includes entries for "virtual bonds" used in computations), but includes the metadata about cashflows on bonds needed to compute yields.
#'

args <- commandArgs(TRUE)
outfile <- args[1]

#' Some helper functions to generate a number of cashflows starting from the redemption date
generate_cashflows <-
    function(redemption,
             interest,
             coupons,
             period = 2,
             face=100,
             specie_interest=TRUE, specie_principal=TRUE)
{
    monthperiod <- 12 %/% 2
    dates <- redemption - months((coupons - 1):0) * monthperiod
    coupon_payment = face * interest / period
    data.frame(date = c(dates, redemption),
               amount = c(rep(coupon_payment, coupons), face),
               specie = c(rep(specie_interest, coupons), specie_principal),
               interest = c(rep(TRUE, coupons), FALSE))
}

generate_cashflows_years <-
    function(redemption,
             interest,
             years,
             period = 2,
             ...)
{
    coupons <- period * years
    generate_cashflows(redemption,
                        interest,
                        coupons,
                        ...)
}

generate_cashflows_period <-
    function(redemption,
             interest,
             issue_date,
             period = 2,
             ...)
{
    monthperiod <- 12 %/% period
    x <- interval(issue + days(1), redemption, tz = "GMT")
    coupons <- abs(x %/% months(monthperiod))
    generate_cashflows(redemption,
                         interest,
                         coupons,
                         period = period,
                         ...)
}

#' stores data on bonds
bonds <- list()

#'
#' ## United States Government
#'
#' ### Sources
#'
#' - Bayley, Rafael Arroyo (1882) *The National Loans of the United States: From July 4, 1776, to June 30, 1880*, U.S. Government Printing Office, <http://books.google.com/books?id=OQ9AAAAAYAAJ>
#' - De Knight, William F. (1900) *History of the currency of the country and of the loans of the United States, from the earliest period to June 30, 1900* <http://books.google.com/books?id=0cQmAQAAMAAJ>.
#' - Annual Reports of the Secretary of the Treasury,
#' - Franklin Nolls, "Treasury Security Knowledge Base", http://franklinnoll.com/Treasury-Securities-Knowledgebase.html
#' - *Bankers' Magazine* issues
#' - *Merchants' Magazine and Commercial Chronicle* issues
#'

#'
#' ### Sixes of 1867 (Loan of 1847)
#'
#' Issued under the Act of Jan 28, 1847.
#' 6 percent interest semiannually, mauturity of 20 years (1 July 1847).
#'
#' - Bayley [p. 72](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA72), [p. 145-146](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA145)
#' - De Knight [p. 72-73](http://books.google.com/books?id=0cQmAQAAMAAJ&pg=PA72)
bonds[["us_6pct_1868_jan"]] <-
    list(cashflows =
         generate_cashflows(as.Date("1868-1-1"),
                            coupons = 20 * 2,
                            interest = 0.06),
         interest = 0.06,
         periods = list(list(month = 1, day = 1),
         list(month = 7, day = 1)),
         maturity_date = as.Date("1868-1-1"),
         issue_date = as.Date("1847-7-1"))

#'
#' ### Sixes of 1868
#'
#' Issued under the Act of 31 March 1848 (9 Stat 217).
#' 6 percent interest payable semi-annually; maturity 20 years (1 July 1868).
#'
#' - Bayley [p. 73](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA73), [p. 147-148](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA147)
#' - De Knight [p. 73-74](http://books.google.com/books?id=0cQmAQAAMAAJ&pg=PA73)
bonds[["us_6pct_1868_jul"]] <-
    list(cashflows =
         generate_cashflows(as.Date("1868-7-1"),
                            coupons = 20 * 2,
                            interest = 0.06),
         interest = 0.06,
         periods = list(list(month = 1, day = 1), list(month = 7, day = 1)),
         maturity_date = as.Date("1868-7-1"),
         issue_date = as.Date("1848-1-1"))

#'
#' ### Treasury Notes of 1861
#'
#' The auction notice implies interest paid semi-annually.
#' But Bayley suggests interest is paid on redemption.
#' Based off the actual [obligation](https://books.google.com/books?id=0cQmAQAAMAAJ&pg=PA79).
#' It looks like interest was payable on Jan 1 and Jul 1, and thus not a note.
#'
#' - Bayley [p. 76-77](https://books.google.com/books?id=Ce4JAAAAIAAJ&pg=PA75)
#' - Annual Report of the Treasury, July 4, 1861 [p. 50](https://fraser.stlouisfed.org/docs/publications/treasar/AR_TREASURY_1861_2.pdf#page=50)
#' - De Knight [p. 79](https://books.google.com/books?id=0cQmAQAAMAAJ&pg=PA79)
#'
bonds[["us_treasury_note_1861"]] <-
  list(cashflows =
         generate_cashflows(as.Date("1863-1-1"),
                            coupons = 2 * 2,
                            interest = 0.06),
       interest = 0.06,
       periods = list(list(month = 1, day = 1), list(month = 7, day = 1)),
       maturity_date = as.Date("1863-1-1"),
       issue_date = as.Date("1861-3-2"))

#'
#' ### Loan of 1843 (Fives of 1853)
#'
#' - Treasury Report 1843 <https://fraser.stlouisfed.org/docs/publications/treasar/AR_TREASURY_1843.pdf#page=34>
#' - Bayley [p. 70](https://books.google.com/books?id=Ce4JAAAAIAAJ&pg=PA70), [p. 142-43](https://books.google.com/books?id=Ce4JAAAAIAAJ&pg=PA142)
#' - De Knight [p. 68](https://books.google.com/books?id=0cQmAQAAMAAJ&pg=PA68)
#'
#' Proposals invited on April 2, 1843
bonds[["us_5pct_1853"]] <-
  list(cashflows =
         generate_cashflows(as.Date("1853-7-1"),
                            coupons = 10 * 2 + 1,
                            interest = 0.05),
       interest = 0.05,
       periods = list(list(month = 1, day = 1), list(month = 7, day = 1)),
       maturity_date = as.Date("1853-7-1"),
       issue_date = as.Date("1843-4-01"))

#'
#' ### Texas Indemnity (Fives of 1865)
#'
#' Issued under the Act of Sept 9, 1850 (9 Stat 447).
#' This was issued to idemnify Texas for the relinquishment of her claims.
#' Interest of 5 percent, payable semi-annually; maturity of 14 years (Jan 1, 1865)
#'
#' - Bayley [p. 73-74](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA73), [148-149](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA148)
#' - De Knight [p. 74-75](http://books.google.com/books?id=0cQmAQAAMAAJ&pg=PA74]
#' - Annual Report of the Treasury 1863, [p. 42](https://fraser.stlouisfed.org/docs/publications/treasar/AR_TREASURY_1863.pdf#page=50)
bonds[["us_texas_indemnity"]] <-
    list(cashflows =
           generate_cashflows_years(as.Date("1865-1-1"),
                                    years = 15, interest = 0.05),
         interest = 0.05,
         periods = list(list(month = 1, day = 1),
                        list(month = 7, day = 1)),
         maturity_date = as.Date("1865-1-1"),
         issue_date = as.Date("1850-1-1"))

#'
#' ### Fives of 1871 (Loan of 1860)
#'
#' Issued under the Act of June 22, 1860 (12 Stat 79).
#' Interest of 5 percent, payable semi-annually; length of 10 years (Jan 1, 1871).
#'
#' - Bayley [p. 75](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA75), [150](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA150)
#' - De Knight [p. 77](http://books.google.com/books?id=0cQmAQAAMAAJ&pg=PA77]
#' - Annual Report of the Treasury 1863, [p. 42](https://fraser.stlouisfed.org/docs/publications/treasar/AR_TREASURY_1863.pdf#page=50)
#'

bonds[["us_5pct_1871"]] <-
    list(cashflows =
         generate_cashflows_years(as.Date("1871-1-1"),
                                  10, interest = 0.05),
         interest = 0.05,
         periods =
         list(list(month = 1, day = 1),
              list(month = 7, day = 1)),
         maturity_date = as.Date("1871-1-1"),
         issue_date = as.Date("1861-1-1"))

#'
#' ### Oregon War Loan
#'
#' Issued under the Act of Mar 2, 1861 (12 Stat 198) to repay the states of Oregon and Washington for expenses in the Indian wars of 1855-56.
#' 6 percent interest paid semi-annually, length of 20 years (Jul 1, 1881).
#'
#' - Bayley [p. 77-78](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA77), [152](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA152)
#' - De Knight [p. 80-81](http://books.google.com/books?id=0cQmAQAAMAAJ&pg=PA80]
#' - Annual Report of the Treasury 1863, [p. 42](https://fraser.stlouisfed.org/docs/publications/treasar/AR_TREASURY_1863.pdf#page=50)
bonds[["us_oregon_war"]] <-
    list(cashflows =
         generate_cashflows_years(as.Date("1881-1-1"),
                                  years = 20, interest = 0.06),
         interest = 0.06,
         periods = list(list(month = 1, day = 1),
         list(month = 7, day = 1)),
         maturity_date = as.Date("1881-1-1"),
         issue_date = as.Date("1861-7-1"))

#'
#' ### Fives of 1874 (Loan of 1858)
#'
#' Authorized under the Act of June 14, 1858.
#' 5 percent interest, payable semi-annually.
#' Length of 15 years (Jan 1, 1874).
#'
#' - Bayley [p. 74](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA74), [149-150](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA149)
#' - De Knight [p. 76](http://books.google.com/books?id=0cQmAQAAMAAJ&pg=PA76]
#' - Annual Report of the Treasury 1863, [p. 42-43](https://fraser.stlouisfed.org/docs/publications/treasar/AR_TREASURY_1863.pdf#page=50)
bonds[["us_5pct_1874"]] <-
    list(cashflows =
         generate_cashflows(as.Date("1874-1-1"),
                            coupons = 15 * 2 + 1,
                            interest = 0.05),
         interest = 0.05,
         periods = list(list(month = 1, day = 1),
         list(month = 7, day = 1)),
         maturity_date = as.Date("1874-1-1"),
         issue_date = as.Date("1858-7-1"))

#'
#' ### Sixes of 1881 (Loan of February 1861)
#'
#' Authorized under the Act of Feb 8, 1861 (12 Stat 129).
#' 6 percent semi-annually; length of 20 years (Jan 1, 1861).
#'
#' - Bayley [p. 76](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA76), [151](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA149)
#' - De Knight [p. 78-79](http://books.google.com/books?id=0cQmAQAAMAAJ&pg=PA78]
#' - Annual Report of the Treasury 1863, [p. 42-43](https://fraser.stlouisfed.org/docs/publications/treasar/AR_TREASURY_1863.pdf#page=50)
#' - Noll, Vol 5, [p. 511](http://franklinnoll.com/Vol_6.pdf#page=512)
bonds[["us_6pct_1881_jan"]] <-
    list(cashflows =
         generate_cashflows_years(as.Date("1881-1-1"),
                                  years = 20,
                                  interest = 0.06),
         interest = 0.06,
         periods = list(list(month = 1, day = 1),
         list(month = 7, day = 1)),
         maturity_date = as.Date("1881-1-1"),
         issue_date = as.Date("1861-1-1"))

#'
#' ### Sixes of 1881 (Loan of July and August 1861)
#'
#' Authorized under the Acts of July 17, 1861 (12 Stat 259) and
#' August 5, 1861.
#' 6 percent semi-annually; length of 20 years (July 1, 1861).
#'
#' - Bayley [p. 78-79](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA78), [152-153](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA152)
#' - De Knight [p. 81-82](http://books.google.com/books?id=0cQmAQAAMAAJ&pg=PA81]
#' - Noll, Vol 5, [p. 512](http://franklinnoll.com/Vol_6.pdf#page=513)
#' - Annual Report of the Treasury 1863, [p. 44-45](https://fraser.stlouisfed.org/docs/publications/treasar/AR_TREASURY_1863.pdf#page=52)
bonds[["us_6pct_1881_jul"]] <-
    list(cashflows =
         generate_cashflows_years(as.Date("1881-7-1"),
                                  years = 20, interest = 0.06),
         interest = 0.06,
         periods = list(list(month = 1, day = 1),
         list(month = 7, day = 1)),
         maturity_date = as.Date("1881-7-1"),
         issue_date = as.Date("1861-7-1"))

#'
#' ### Seven-Thirties of 1861
#'
#' Authorized under the Act of July 17, 1861 (12 Stat 259).
#' 7.30 percent payable semiannually in specie;
#' length of 3 years, although convertible to a longer bond.
#' There were two issues:
#'
#' - Redeemable on 1864-08-19
#' - Redeemable on 1864-10-01
#'
#' At maturity seven thirties could be either converted to face value in *legal tender* (not specie),
#' or converted to 6's of 1881 (maturing on July 1, 1881) at par upon maturity.
#' If converted to a 6 percent bond, the 1st interest payment of the 6 percent bond will be July 1864.
#'
#' - Beyeley [p. 78](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA78), [154-155](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA154)
#' - De Knight [p. 83](http://books.google.com/books?id=0cQmAQAAMAAJ&pg=PA83)
#' - Annual Report of the Treasury 1863, [p. 44-45](https://fraser.stlouisfed.org/docs/publications/treasar/AR_TREASURY_1863.pdf#page=52)
#' - Noll, Vol 6, [p. 156](http://franklinnoll.com/Vol_6.pdf#page=157)
#' - [Commerical Chronicle and Review, July 1867](http://books.google.com/books?id=pk81AQAAMAAJpg=PA75)
#' - [Commercial and Financial Chronicle, Aug 17, 1867](http://books.google.com/books?id=e3FAAQAAMAAJ&dq=seven%20thirties&pg=PA198)
#' - [Bankers' Magazine, June 1864, p. 940](http://books.google.com/books?id=D14mAQAAIAAJ&pg=940), "Notice to the Holders of the Three Years 7-30 Notes".
#' - Interest payable in specie, but principal payable in *currency*. George Harrington, "Payment of the Five Twenties in Gold", [Banker's Magazine](http://books.google.com/books?id=D14mAQAAIAAJ&pa=10).
#'
bonds[["us_seven_thirties_1864_aug"]]  <-
    list(cashflows =
         generate_cashflows_years(as.Date("1864-8-19"),
                                 years = 3,
                                 interest = 0.073,
                                 specie_principal = FALSE),
         interest = 0.073,
         periods = list(list(month = 2, day = 19),
         list(month = 8, day = 19)),
         maturity_date = as.Date("1864-8-19"),
         issue_date = as.Date("1861-8-19"))

bonds[["us_seven_thirties_1864_aug_option"]]  <-
    local({
        cashflows <-
            rbind(filter(bonds[["us_seven_thirties_1864_aug"]]$cashflows, specie),
                  filter(bonds[["us_6pct_1881_jul"]]$cashflows,
                         date > as.Date("1864-10-01")))
        list(cashflows = cashflows,
             interest = NA,
             maturity_date = as.Date("1881-7-1"),
             issue_date = as.Date("1861-8-19"))
    })

## Redeemable on 1864-10-1
bonds[["us_seven_thirties_1864_oct"]]  <-
    list(cashflows =
         generate_cashflows_years(as.Date("1864-10-01"),
                                 years = 3,
                                 interest = 0.073,
                                 specie_principal = FALSE),
         interest = 0.073,
         periods = list(list(month = 4, day = 1),
         list(month = 10, day = 1)),
         maturity_date = as.Date("1864-10-01"),
         issue_date = as.Date("1861-10-01"))

bonds[["us_seven_thirties_1864_oct_option"]]  <-
    local({
        cashflows <-
            rbind(filter(bonds[["us_seven_thirties_1864_oct"]]$cashflows, interest),
                  filter(bonds[["us_6pct_1881_jul"]]$cashflows,
                         date > as.Date("1864-10-01")))
        list(cashflows = cashflows,
             interest = NA,
             maturity_date = as.Date("1881-7-1"),
             issue_date = as.Date("1861-10-01"))
    })

#'
#' ### Seven Thirties of 1864
#'
#' Authorized under the Act of June 30, 1864 (13 Stat 218).
#' Paid interest of 7.3% semiannually, with a maturity of 3 years (August 15, 1867).
#'
#' These paid 7.30 percent semiannually in lawful currency, with the option on redemption of exchanging it for a bond, redeemable at the pleasure of Government after 5 years, and payable twenty years from July 15, 1868, with interest a 6 percent payable semiannually in coin.
#'
#' - Noll , vol 6, [p. 157](http://www.franklinnoll.com/Vol_6.pdf#page=158)
#' - De Knight [p. 97-98](http://books.google.com/books?id=0cQmAQAAMAAJ&pg=PA97]
#' - Bayley [p. 85-87](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA85), [165-166](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA165)
#' - Annual Report of the Treasury 1867, [p. LIV-LV](https://fraser.stlouisfed.org/docs/publications/treasar/AR_TREASURY_1867.pdf#page=58)
#'
bonds[["us_seven_thirties_1867_aug"]]  <-
    list(cashflows =
         generate_cashflows_years(as.Date("1867-08-15"),
                                  years = 3,
                                  interest = 0.073,
                                  specie_interest = FALSE,
                                  specie_principal = FALSE),
         interest = 0.073,
         periods = list(list(month = 2, day = 15),
         list(month = 8, day = 15)),
         maturity_date = as.Date("1867-08-15"),
         issue_date = as.Date("1864-08-15"))

for (i in 5:20) {
    bonds[[sprintf("us_seven_thirties_1867_aug_option_%d_year", i)]]  <-
        local({
            cf1 <- bonds[["us_seven_thirties_1867_aug"]]$cashflow
            yyyy <- i
            maturity <- as.Date("1867-08-15") + years(yyyy)
            cashflows <-
                rbind(cf1[1:(nrow(cf1) - 1), ],
                      generate_cashflows_years(maturity,
                                               years = yyyy,
                                               interest = 0.06))
            list(cashflows = cashflows,
                 interest = NA,
                 periods = list(list(month = 2, day = 15),
                 list(month = 8, day = 15)),
                 maturity_date = maturity,
                 issue_date = as.Date("1864-08-15"))
        })
}

#'
#' ### Seven-thirties of 1865
#'
#' Issued under the Act of March 3, 1865 (13 Stat 468).
#' These notes had a maturity of 3 years and yielded 7.3% payable in lawful currency.
#' On maturity, the principal would be paid in lawful currency or the note could  be converted to a 6 percent 5-20 at par; interest and principal payable in specie.
#'
#' There were two issues: on maturity on 15 June 1865, and one maturing on 15 July 1865.
#'
#' - Noll, Vol 6, [p. 158](http://franklinnoll.com/Vol_6.pdf#page=159)
#' - De Knight [p. 97-98](http://books.google.com/books?id=0cQmAQAAMAAJ&pg=PA97]
#' - Bayley [p. 85-87](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA85), [p. 165-66](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA165)
#' - Annual Report of the Treasury 1867, [p. LIV-LV](https://fraser.stlouisfed.org/docs/publications/treasar/AR_TREASURY_1867.pdf#page=58)
#'
bonds[["us_seven_thirties_1868_jun"]]  <-
    list(cashflows =
         generate_cashflows_years(as.Date("1868-06-15"),
                                  years = 3,
                                  interest = 0.073,
                                  specie_interest = FALSE,
                                  specie_principal = FALSE),
         interest = 0.073,
         periods = list(list(month = 6, day = 15),
         list(month = 12, day = 15)),
         maturity_date = as.Date("1868-06-15"),
         issue_date = as.Date("1865-08-15"))

for (i in 5:20) {
    bonds[[sprintf("us_seven_thirties_1868_jun_option_%d_year", i)]]  <-
        local({
            cf1 <- bonds[["us_seven_thirties_1868_jun"]]$cashflow
            yyyy <- i
            maturity <- as.Date("1868-06-15") + years(yyyy)
            cashflows <-
                rbind(cf1[1:(nrow(cf1) - 1), ],
                      generate_cashflows_years(maturity,
                                               years = yyyy,
                                               interest = 0.06))
            list(cashflows = cashflows,
                 interest = NA,
                 periods = list(list(month = 2, day = 15),
                 list(month = 8, day = 15)),
                 maturity_date = maturity,
                 issue_date = as.Date("1865-06-15"))
        })
}

bonds[["us_seven_thirties_1868_jul"]]  <-
    list(cashflows =
         generate_cashflows_years(as.Date("1868-07-15"),
                                  years = 3,
                                  interest = 0.073,
                                  specie_interest = FALSE,
                                  specie_principal = FALSE),
         interest = 0.073,
         periods = list(list(month = 6, day = 15),
         list(month = 12, day = 15)),
         maturity_date = as.Date("1868-07-15"),
         issue_date = as.Date("1865-07-15"))

for (i in 5:20) {
    bonds[[sprintf("us_seven_thirties_1868_jul_option_%d_year", i)]]  <-
        local({
            cf1 <- bonds[["us_seven_thirties_1868_jul"]]$cashflow
            yyyy <- i
            maturity <- as.Date("1868-07-15") + years(yyyy)
            cashflows <-
                rbind(cf1[1:(nrow(cf1) - 1), ],
                      generate_cashflows_years(maturity,
                                               years = yyyy,
                                               interest = 0.06))
            list(cashflows = cashflows,
                 interest = NA,
                 periods = list(list(month = 1, day = 15),
                 list(month = 7, day = 15)),
                 maturity_date = maturity,
                 issue_date = as.Date("1865-07-15"))
        })
}


#'
#' ### Ten-Forties of 1864
#'
#' Issued under the act of March 3, 1864 (13 Stat 13).
#' Interest of 5 percent, payable semiannually.
#' Length of 40 years, but callable by the government after 10 years.
#'
#' To account for the callable option on this bond, I generate bonds for all redemption years between 10 and 40.
#'
#' - Noll, Vol. 6, [p. 178](http://www.franklinnoll.com/Vol_6.pdf#page=179).
#' - De Knight [p. 93-94](http://books.google.com/books?id=0cQmAQAAMAAJ&pg=PA93]
#' - Bayley, [p. 84-85](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA84), [p. 164](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA164)
#' - Annual Report of the Treasury 1867, [p. LII-LIII](https://fraser.stlouisfed.org/docs/publications/treasar/AR_TREASURY_1867.pdf#page=56)
#'
for (i in 10:40) {
    issue <- as.Date("1864-3-1")
    maturity <- issue + years(i)
    yr <- year(maturity)
    bondname <- sprintf("us_ten_forty_%d", yr)
    bonds[[bondname]] <-
        list(cashflows =
             generate_cashflows_years(maturity,
                                      years = i,
                                      interest = 0.05),
             interest = 0.05,
             periods = list(list(month = 3, day = 1),
             list(month = 9, day = 1)),
             maturity_date = maturity,
             issue_date = issue)
}

#'
#' ### Five-Twenties of 1862
#'
#' Authorized under the Acts of Feb 25, 1862, 3 March 1864 and 28 January 1865.
#' 6 percent interest at a maturity of 20 years, callable by the government after 5.
#' There were some questions as to whether the principal was payable in specie, but several explicit statements by the Treasury that it was.
#'
#' To account for the call option on this bond, entries are generated for all cashflows for 5 to 20 years.
#'
#' - Interest and principal is payable in specie. George Harrington, "Payment of the Five Twenties in Gold", [Banker's Magazine](http://books.google.com/books?id=D14mAQAAIAAJ&pa=10).
#' - De Knight [p. 85-86](http://books.google.com/books?id=0cQmAQAAMAAJ&pg=PA85)
#' - Bayley [79-80](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA79), [155-56](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA155)
#' - Annual Report of the Treasury 1867, [p. LII-LIII](https://fraser.stlouisfed.org/docs/publications/treasar/AR_TREASURY_1867.pdf#page=56)
#' - Noll, Vol 5, [p. 297](http://franklinnoll.com/Vol_5.pdf#page=298).
#'
for (i in 5:20) {
    issue <- as.Date("1862-5-1")
    maturity <- issue + years(i)
    yr <- year(maturity)
    bondname <- sprintf("us_five_twenty_of_1862_redeemed_%d", yr)
    bonds[[bondname]] <-
        list(cashflows =
             generate_cashflows_years(maturity,
                                      years = i,
                                      interest = 0.06),
             interest = 0.06,
             periods = list(list(month = 5, day = 1),
             list(month = 11, day = 1)),
             maturity_date = maturity,
             issue_date = issue)
}

#'
#' ### Five Twenties of June 1864
#'
#' Authorized under the Acts of 3 March and 30 June 1864.
#' These were issued under two acts, but both had the same maturity.
#' 6 percent interest at a maturity of 20 years, callable by the government after 5.
#'
#' To account for the call option on this bond, entries are generated for all cashflows for 5 to 20 years.
#'
#' - De Knight [p. 94-95](http://books.google.com/books?id=0cQmAQAAMAAJ&pg=PA94)
#' - Bayley [84-86](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA84), [164-165](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA164)
#' - Noll, Vol 5, [p. 298-299](http://franklinnoll.com/Vol_5.pdf#page=300).
#' - Annual Report of the Treasury 1867, [p. LII-LIII](https://fraser.stlouisfed.org/docs/publications/treasar/AR_TREASURY_1867.pdf#page=56)
#'
for (i in 5:20) {
    issue <- as.Date("1864-11-1")
    maturity <- issue + years(i)
    yr <- year(maturity)
    bondname <- sprintf("us_five_twenty_of_1864_redeemed_%d", yr)
    bonds[[bondname]] <-
        list(cashflows =
             generate_cashflows_years(maturity,
                                      years = i,
                                      interest = 0.06),
             interest = 0.06,
             periods = list(list(month = 5, day = 1),
             list(month = 11, day = 1)),
             maturity_date = maturity,
             issue_date = issue)
}

#'
#' ### Five-Twenties of 1865
#'
#' Authorized under the Acts of 3 March 1865 and 12 April 1866.
#' 6 percent interest at a maturity of 20 years, callable by the government after 5.
#'
#' To account for the call option on this bond, entries are generated for all cashflows for 5 to 20 years.
#'
#' - De Knight [p. 98-99](http://books.google.com/books?id=0cQmAQAAMAAJ&pg=PA98)
#' - Bayley [167](http://books.google.com/books?id=OQ9AAAAAYAAJ&pg=PA167)
#' - Noll, Vol 5, [p. 300](http://franklinnoll.com/Vol_5.pdf#page=301).
#' - Annual Report of the Treasury 1867, [p. LIV-LV](https://fraser.stlouisfed.org/docs/publications/treasar/AR_TREASURY_1867.pdf#page=58)
for (i in 5:20) {
    issue <- as.Date("1865-11-1")
    maturity <- issue + years(i)
    yr <- year(maturity)
    bondname <- sprintf("us_five_twenty_of_1865_redeemed_%d", yr)
    bonds[[bondname]] <-
        list(cashflows =
             generate_cashflows_years(maturity,
                                      years = i,
                                      interest = 0.06),
             interest = 0.06,
             periods = list(list(month = 5, day = 1),
             list(month = 11, day = 1)),
             maturity_date = maturity,
             issue_date = issue)
}

#' Set issuer to US for all US govt bonds
for (i in grep("^us_", names(bonds), value = TRUE)) {
    bonds[[i]][["issuer"]] <- "US"
}

#'
#' ## State and City Bonds
#'
#' - Messrs Thomas Denny & Co (1869) "State Stocks in 1868: The Amount Outstanding; The Annual Interest; Interest; When Payable and When Due" in *Bankers' Magazine* April 1869, <http://books.google.com/books?id=2lgmAQAAIAAJ&pg=PA801>.
#' - "City, County, and Other Bonds" from *Bankers' Magazine*, October 1857, <http://books.google.com/books?id=g2QmAQAAIAAJ&pg=PA332>.
#'

#' ### California
#'
#' #### 7 percent, 1870
#'
#' Interest of 7 percent, payable semi-annually.
#' Matures in 1870, unknown when it was issued.
#'
#' - *Bankers' Magazine*, October 1857, <http://books.google.com/books?id=g2QmAQAAIAAJ&pg=PA332>
#'
bonds[["california_7pct_1870"]] <-
    list(cashflows =
         generate_cashflows_years(as.Date("1870-7-1"),
                                  years = 35,
                                  interest = 0.07),
         interest = 0.07,
         maturity_date = as.Date("1870-7-1"),
         issue_date = NA,
         issuer = "California",
         periods = list(list(month = 1, day = 1),
         list(month = 7, day = 1)))
#'
#' #### 7 percent, 1877
#'
#' Interest of 7 percent, payable semi-annually.
#' Matures in 1877, unknown when it was issued.
#'
#' - Denny and Co.
#'
bonds[["california_7pct_1877"]] <-
    list(cashflows =
         generate_cashflows_years(as.Date("1877-7-1"),
                                  years = 35,
                                  interest = 0.07),
         interest = 0.07,
         maturity_date = as.Date("1877-7-1"),
         issue_date = NA,
         issuer = "California",
         periods = list(list(month = 1, day = 1),
         list(month = 7, day = 1)))

#'
#' ### Indiana
#'
#' #### 5 percent
#'
#' 5 percent, payable semiannually (Jan, Jul).
#' Redemption is unknown.
#'
#' This is not the 2.5 percent or Canal loans.
#'
#' - http://books.google.com/books?id=g2QmAQAAIAAJ&pg=PA332 : no redemption years, payable Jan/Jul.
#' - Denny & Co: redemption various, payable various.
#'
for (yyyy in 1869:1892) {
    bondname <- sprintf("indiana_5pct_%d", yyyy)
    bonds[[bondname]] <-
        list(cashflows =
             generate_cashflows_years(as.Date(sprintf("%d-7-1", yyyy)),
                                      years = 40,
                                      interest = 0.05),
             maturity_date = as.Date(sprintf("%s-7-1", yyyy)),
             issue_date = NA,
             interest = 0.05,
             periods = list(list(month = 1, day = 1), list(month = 7, day = 1)))
}

#'
#' #### 6 percent (War Loan)
#'
#' 6 percent, payable semi-annually (May, Nov).
#' Redeemable in 1881.
#' Known to be issued in 1861 to fund the war.
#'
#' - *Bankers' Magazine*, Vol. 16, p. 79. http://books.google.com/books?id=B10mAQAAIAAJ&pg=PA79
#' - Denny & Co.
#'
bonds[["indiana_6pct_1881"]] <-
    list(cashflows =
         generate_cashflows_years(as.Date("1881-11-1"),
                                  years = 20,
                                  interest = 0.06),
         maturity_date = as.Date(sprintf("%s-11-01", 1881)),
         issue_date = as.Date("1861-11-1"),
         interest = 0.06,
         periods = list(list(month = 5, day = 1),
         list(month = 11, day = 1)))

#'
#' ### Georgia
#'
#' 6 percent interest, payable semi-annually.
#'
#' - http://books.google.com/books?id=g2QmAQAAIAAJ&pg=PA332 : redeemable 1872, payable Jan/Jul.
#' - Denny & Co: redeemable 1868-1887, payable Jan/Jul.
#'
bonds[["georgia_6pct_1872"]] <-
    list(cashflows =
         generate_cashflows_years(as.Date("1872-7-1"),
                                  years = 35,
                                  interest = 0.06),
         maturity_date = as.Date("1872-07-1"),
         issue_date = NA,
         interest = 0.06,
         issuer = "Georgia",
         periods = list(list(month = 1, day = 1),
         list(month = 7, day = 1)))

#'
#' ### Kentucky
#'
#' #### 6 percent 1868-1885
#'
#' In *Bankers' Magazine* quotes of prices, no redemption dates are listed.
#'
#' - http://books.google.com/books?id=g2QmAQAAIAAJ&pg=PA332 Lists years 1869-72, payable in Jan, Jul.
#' - Economist 24, 1861 has a redemption year of 1868.
#' - Denny lists redemption dates between 1868-1885.
#'
for (year in 1868:1885) {
    bondname <- sprintf("kentucky_6pct_%d", year)
    bonds[[bondname]] <-
        list(cashflows =
             generate_cashflows_years(as.Date(sprintf("%d-7-1", year)),
                                years = 35,
                                interest = 0.06),
             maturity_date = as.Date(sprintf("%d-7-1", year)),
             issue_date = NA,
             interest = 0.06,
             issuer = "Kentucky",
             periods = list(list(month = 1, day = 1),
             list(month = 7, day = 1)))
}

#'
#'
#' ### Louisiana
#'
#' #### 6 percent
#'
#' 6 percent payable semiannually. Various and
#'
#' No redemption years are listed in the *Bankers' Magazine* quotes.
#'
#' - http://books.google.com/books?id=g2QmAQAAIAAJ&pg=PA332 lists diverse.
#' - Denny lists various
#'
for (year in 1869:1892) {
    bondname <- sprintf("louisiana_6pct_%d", year)
    bonds[[bondname]] <-
        list(cashflows =
             generate_cashflows_years(as.Date(sprintf("%d-7-1", year)),
                                      years = 35,
                                      interest = 0.06),
             maturity_date = as.Date(sprintf("%d-7-1", year)),
             issue_date = NA,
             interest = 0.06,
             issuer = "Louisiana",
             periods = list(list(month = 1, day = 1),
             list(month = 7, day = 1)))
}

#'
#' ### Missouri
#'
#' Interest of 6 percent payable semiannually (Jan / Jul).
#' Due on various years.
#'
#' - Denny & Co. : redemption 1868-1890
#' - http://books.google.com/books?id=g2QmAQAAIAAJ&pg=PA332 : redemption, 1872.
#'
bonds[["missouri_6pct_1872"]] <-
    list(cashflows =
         generate_cashflows_years(as.Date("1872-7-1"),
                                 years = 35,
                                 interest = 0.06),
         maturity_date = as.Date("1872-7-1"),
         issue_date = NA,
         interest = 0.06,
         issuer = "Missouri",
         periods = list(list(month = 1, day = 1),
         list(month = 7, day = 1)))

#'
#' ### North Carolina
#'
#' Interest of 6 percent payable semiannually.
#'
#' - *Bankers' Magazine* 1857, http://books.google.com/books?id=g2QmAQAAIAAJ&pg=PA332 : redemption year 1873.
#' - Denny & Co : redemption year 1868.
#'
bonds[["north_carolina_6pct_1873"]] <-
    list(cashflows =
         generate_cashflows_years(as.Date("1873-7-1"),
                                  years = 35,
                                  interest = 0.06),
         maturity_date = as.Date("1873-7-1"),
         issue_date = NA,
         interest = 0.06,
         issuer = "North Carolina",
         periods = list(list(month = 1, day = 1),
         list(month = 7, day = 1)))

#'
#' ### Ohio
#'
#' #### 6 percent, 1875
#'
#' 6 percent, payable semi-annually, matures in 1875.
#' Jan / July assumed because other
#'
#' - Denny and Co.
#' - http://books.google.com/books?id=g2QmAQAAIAAJ&pg=PA332
#'
bonds[["ohio_6pct_1875"]] <-
    list(cashflows =
         generate_cashflows_years(as.Date("1875-7-1"),
                                  years = 35,
                                  interest = 0.06),
         interest = 0.06,
         maturity_date = as.Date("1875-7-1"),
         issue_date = NA,
         issuer = "Ohio",
         periods = list(list(month = 1, day = 1),
         list(month = 7, day = 1)))

#'
#' #### 6 percent, 1886
#'
#' 6 percent, payable semi-annually, matures in 1886.
#'
#' - Denny and Co.
#' - http://books.google.com/books?id=g2QmAQAAIAAJ&pg=PA332
#'
bonds[["ohio_6pct_1886"]] <-
    list(cashflows =
         generate_cashflows_years(as.Date("1886-7-1"),
                                  years = 35,
                                  interest = 0.06),
         interest = 0.06,
         maturity_date = as.Date("1886-7-1"),
         issue_date = NA,
         issuer = "Ohio",
         periods = list(list(month = 1, day = 1),
         list(month = 7, day = 1)))

#'
#' ### Pennsylvania
#'
#' #### 5 percent
#'
#' 5 percent interest paid semiannually (Feb, Aug).
#'
#' This is different than the "War Loan" issued to fund the war and had 6 percent interest.
#'
#' - *Bankers' Magazine*, Vol 15, p. 166. http://books.google.com/books?id=KVwmAQAAIAAJ&pg=PA166 : explicitly states that interest is paid in August.
#' - http://books.google.com/books?id=g2QmAQAAIAAJ&pg=PA332 does not list a redemption year.
#' - Denny & Co. list various interst rates, but a redemption of 1871.
bonds[["pennsylvania_5pct_1871"]] <-
    list(cashflows =
         generate_cashflows_years(as.Date("1871-8-1"),
                                  year = 35,
                                  interest = 0.05),
         maturity_date = as.Date("1871-8-1"),
         issue_date = NA,
         interest = 0.05,
         issuer = "Pennsylvania",
         periods = list(list(month = 2, day = 1),
         list(month = 8, day = 1)))

#'
#' ### Tennessee
#'
#' 6 percent interest, payable semiannually (unknown or various months, so Jan / Jul will be assumed).
#'
#' - http://books.google.com/books?id=g2QmAQAAIAAJ&pg=PA332. redemption various, payable various.
#' - Denny & Co: redemption 1885-1892, payable various.
#'
for (year in 1885:1892) {
    bondname <- sprintf("tennessee_6pct_%d", year)
    bonds[[bondname]] <-
        list(cashflows =
             generate_cashflows_years(as.Date(sprintf("%d-7-1", year)),
                                     years = 35,
                                     interest = 0.06),
             maturity_date = as.Date(sprintf("%s-7-1", year)),
             issue_date = NA,
             interest = 0.06,
             issuer = "Tennessee",
             periods = list(list(month = 1, day = 1),
             list(month = 7, day = 1)))
}


#'
#' ### Virginia
#'
#' 6 percent interest, payable semi-annually (Jan, Jul)
#' Various redemption years.
#'
#' - http://books.google.com/books?id=g2QmAQAAIAAJ&pg=PA332, redemption 1885-1890.
#' - Davis and Pecquet (1990), p. 147 use 1887 (average maturity of long-term issues)
#' - Denny & Co list "various" redemption years.
for (year in 1885:1890) {
    bondname <- sprintf("virginia_6pct_%d", year)
    bonds[[bondname]] <-
        list(cashflows =
             generate_cashflows_years(as.Date(sprintf("%d-7-1", year)),
                                      years = 40,
                                      interest = 0.06),
             maturity_date = as.Date(sprintf("%s-7-1", year)),
             issue_date = NA,
             interest = 0.06,
             issuer = "Virginia",
             periods = list(list(month = 1, day = 1),
             list(month = 7, day = 1)))
}


#'
#' ## Confederate
#'

#' ## Confederate 5 million loan
#'
#' Act of Feb 28, 1861, Specie Loan
#'
#' :authorized: unlimited
#' :maturity: 10 years (9-1-1871), callable after 5 years
#' :interest: 8 per cent, coupons receivable for export dues
#' :total issued: 15 million
#'

## for (year in 5:10) {
##     bondname <- sprintf("confed_15mn_%d", year)
##     bonds[[bondname]] <-
##         list(cashflows = generate_cashflow_years(as.Date("1861-9-1"), year * 2, 4),
##              interest = 0.08,
##              periods = list(list(month = 3, day = 1), list(month = 9, day = 1)),
##              issued = as.Date("1861-9-1"))
## }

#' Confederate 100 million
#' ------------------------
#'
#' Act of Aug 19, 1861
#'
#' :authorized: 100 mn
#' :maturity: various dates, Jul 1, 1864 (3 yrs)-Jul 1, 1881 (20 yrs)
#' :interest: 8 per cent in liue of authorized by Act of May 16, 1861
#' :total issued:  $99,570,550
#' 

## for (year in 3:20) {
##     bondname <- sprintf("confed_100mn_%d", year)
##     bonds[[bondname]] <-
##         list(cashflows = generate_cashflow_2(as.Date("1861-7-1"), year * 2, 4),
##              interest = 0.08,
##              periods = list(list(month = 1, day = 1), list(month = 7, day = 1)),
##              issued = as.Date("1861-7-1"))
## }

#' Richmond
#' ------------
#'
#' Assume date of 1879 as in Davis and Pecquet (1990)

#' Petersburg
#' -------------
#'
#' Don't have a date, so assume the same date at Richmond.

cat(toJSON(bonds, asIs = FALSE), "\n", file = outfile)
