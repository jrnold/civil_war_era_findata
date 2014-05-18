library("jsonlite")
library("lubridate")

#' This generates metadata about each bond issue.
#'
#' ## Noll
#'
#' Each entry in (Treasury Securities Knowledge Base Contains)[http://www.franklinnoll.com/Treasury-Securities-Knowledgebase.html].
#'
#' - Description
#' - Authorizing Acts
#' - Liability: e.g. "Public Debt"
#' - Issued for: e.g. "United States Treasury"
#' - Instrument Type: "Note", "Bond"
#' - Conditions
#'
#'    - Interest
#'    - Maturity
#'    - Redeemable
#'    - Callable
#'    - Payable: gold coin, specie
#'    - Sold at:
#'
#' - Denominiations
#' - Issues
#' - Source
#'
#' ## Early NYSE Govt Securities Data
#'
#' From Yale International Center for Finanace data [Old New York Stock Exchange 1815-1925](http://som.yale.edu/faculty-research/our-centers-initiatives/international-center-finance/data/historical-newyork) data on 
#'
#' - name:
#' - page: on Bayley
#' - date of statute
#' - length: in years
#' - due:
#' - amount authorized
#' - amount issued
#' - sold at
#' - interest rate
#' - interest payment; semi-annual
#' 
#'
#'
#' ## Sources
#'
#' - Bayley
#' - DeKnight
#' - Annual Reports of the Secretary of the Treasury
#' - Franklin Nolls, http://franklinnoll.com/Treasury-Securities-Knowledgebase.html, "Treasury Security Knowledge Base"
#' 

args <- commandArgs(TRUE)
outfile <- args[1]

setMethod("toJSON", "Date",
          function(x, ...) callGeneric(format(x, "%Y-%m-%d"), ...))

#' Generate a number of cashflows startint from the redemption date
generate_cashflow_1 <- function(redemption, n_coupons, coupon,
                                face=100, period = 6) {
    dates <- sort(redemption - months(0:(n_coupons - 1) * period))
    cf <- c(rep(coupon, length(dates) - 1), face + coupon)
    data.frame(date = dates, amount = cf)
}

#' Generate a number of cashflows starting from the issue date
generate_cashflow_2 <- function(issue, n_coupons, coupon,
                                face=100, period = 6) {
    dates <- sort(issue + months(1:n_coupons * period))
    cf <- c(rep(coupon, length(dates) - 1), face + coupon)
    data.frame(date = dates, amount = cf)
}


#' - ``coupons`` stores cashflows
#' - ``bonds`` stores relationship between bonds.
coupons <- list()

#'
#' # US Bonds
#' 

#'
#' ## Sixes of 1867-68
#'
#' Reedemable on Jan 1, 1868
#' Issued under the loan of 1847.
#' 
coupons$us_sixes_18680101 <-
    list(cashflows = generate_cashflow_1(as.Date("1868-1-1"), 20 * 2, 3),
         interest = 0.06,
         periods = list(c(1, 1), c(7, 1)),
         maturity_date = as.Date("1868-1-1"),
         issue_date = as.Date("1847-7-1"))

#'
#' Reedemable on Jul 1, 1868
#' 
#' 
coupons$us_sixes_18680701 <-
    list(cashflows = generate_cashflow_1(as.Date("1868-7-1"), 20 * 2, 3),
         interest = 0.06,
         periods = list(list(month = 1, day = 1), list(month = 7, day = 1)),
         maturity_date = as.Date("1868-7-1"),
         issue_date = as.Date("1848-1-1"))

#'
#' ## Texas Indemnity (Fives of 1865)
#'
#' Issued under the Act of Sept 9, 1850.
#' Authorized the issue of 10 million at 5 percent interest, redeemable in 15 years (Jan 1, 1865).
#' 5 million were issued.
#' This was issued to idemnify Texas for the relinquishment of her claims.
#' 
#' - https://fraser.stlouisfed.org/docs/publications/treasar/AR_TREASURY_1863.pdf p. 42
coupons[["us_texas_indemnity"]] <-
    list(cashflows = generate_cashflow_1(as.Date("1865-1-1"), 15 * 2, 2.5),
         interest = 0.05,
         periods = list(list(month = 1, day = 1), list(month = 7, day = 1)),
         maturity_date = as.Date("1865-1-1"),
         issue_date = as.Date("1850-1-1"))

#'
#' ## Sixes of 1871 (Loan of 1860)
#'
#' Issued under the Act of Dec 17, 1860.
#' Interest of 5 percent per annum; 10 year redemption (Jan 1, 1871).
#' 21 mn aurhorized but only 7,022,000 issued.
#' 
#' - https://fraser.stlouisfed.org/docs/publications/treasar/AR_TREASURY_1863.pdf p. 42
#' 
coupons[["us_fives_18710101"]] <-
    list(cashflows = generate_cashflow_1(as.Date("1871-1-1"), 10 * 2, 2.5),
         interest = 0.05,
         periods = list(list(month = 1, day = 1), list(month = 7, day = 1)),
         maturity_date = as.Date("1871-1-1"),
         issue_date = as.Date("1861-1-1"))

#'
#' ## Oregon War Loan
#'
#' Issued under the Act of Mar 2, 1861 to repay the states of Oregon and Washington for expenses in the Indian wars of 1855-56.
#' 6 percent interest, 20 year maturity. Jul 1, 1881.
#' 2.8 mn authorized, only about 1.1 mn issued. 
#' 
coupons[["us_oregon_war"]] <-
    list(cashflows = generate_cashflow_1(as.Date("1881-1-1"), 20 * 2, 3),
         interest = 0.06,
         periods = list(list(month = 1, day = 1), list(month = 7, day = 1)),
         maturity_date = as.Date("1881-1-1"),
         issue_date = as.Date("1861-7-1"))

#'
#' ## Fives of 1874
#'
coupons$us_fives_18740101 <-
    list(cashflows = generate_cashflow_1(as.Date("1874-1-1"), 15 * 2 + 1, 2.5),
         interest = 0.05,
         periods = list(list(month = 1, day = 1), list(month = 7, day = 1)),
         maturity_date = as.Date("1874-1-1"),
         issue_date = as.Date("1858-7-1"))

#' Sixes of 1881
#' ------------------
#'
#' Redeemable in Jan

coupons$us_sixes_18810101 <-
    list(cashflows = generate_cashflow_1(as.Date("1881-1-1"), 20 * 2, 3),
         interest = 0.06,
         periods = list(list(month = 1, day = 1), list(month = 7, day = 1)),
         maturity_date = as.Date("1881-1-1"),
         issue_date = as.Date("1861-1-1"))

#' Redeemable in July

coupons$us_sixes_18810701 <-
    list(cashflows = generate_cashflow_1(as.Date("1881-7-1"), 20 * 2, 3),
         interest = 0.06,
         periods = list(list(month = 1, day = 1), list(month = 7, day = 1)),
         maturity_date = as.Date("1881-7-1"),
         issue_date = as.Date("1861-7-1"))

#'
#' ## Seven-Thirties of 1861
#'
#' - Redeemable on 1864-08-19
#' - Redeemable on 1864-10-01
#' 
#'
#' Seven thirties were convertible to 6's of 1881 at par upon maturity.
#'
#' - [Commerical Chronicle and Review, July 1867](http://books.google.com/books?id=pk81AQAAMAAJpg=PA75)
#' - [Commercial and Financial Chronicle, Aug 17, 1867](http://books.google.com/books?id=e3FAAQAAMAAJ&dq=seven%20thirties&pg=PA198)
#' - [Banders Magainze, June 1864, p. 940](http://books.google.com/books?id=D14mAQAAIAAJ&pa=940), "Notice to the Holders of the Three Years 7-30 Notes".
#' - Interest payable in specie, but principal payable in *currency*. George Harrington, "Payment of the Five Twenties in Gold", [Banker's Magazine](http://books.google.com/books?id=D14mAQAAIAAJ&pa=10).
#' 
coupons$us_seven_thirties_18640819  <-
    list(cashflows = generate_cashflow_1(as.Date("1864-8-19"), 3 * 2, 7.3 / 2),
         interest = 0.073,
         periods = list(list(month = 2, day = 19), list(month = 8, day = 19)),
         maturity_date = as.Date("1864-8-19"),
         issue_date = as.Date("1861-8-19"))

## Redeemable on 1864-10-1
coupons$us_seven_thirties_18641001  <-
    list(cashflows = generate_cashflow_1(as.Date("1864-10-1"), 3 * 2, 7.3 / 2),
         interest = 0.073,
         periods = list(list(month = 4, day = 1), list(month = 10, day = 1)),
         maturity_date = as.Date("1864-10-01"),
         issue_date = as.Date("1861-10-1"))


#' Seven Thirties of 1864 and 1865
#' (Acts of June 30, 1864 and March 3, 1865)
#'
#' - 1867-8-15
#' - 1868-6-15
#' - 1868-7-15


coupons$us_seven_thirties_18670815  <-
    list(cashflows = generate_cashflow_1(as.Date("1867-08-15"), 3 * 2, 7.3 / 2),
         interest = 0.073,
         periods = list(list(month = 2, day = 15), list(month = 8, day = 15)),
         maturity_date = as.Date("1867-08-15"),
         issue_date = as.Date("1863-08-15"))

coupons$us_seven_thirties_18680615  <-
    list(cashflows = generate_cashflow_1(as.Date("1868-06-15"), 3 * 2, 7.3 / 2),
         interest = 0.073,
         periods = list(list(month = 12, day = 15), list(month = 6, day = 15)),
         maturity_date = as.Date("1868-06-15"),
         issue_date = as.Date("1865-6-15"))

coupons$us_seven_thirties_18680715  <-
    list(cashflows = generate_cashflow_1(as.Date("1868-07-15"), 3 * 2, 7.3 / 2),
         interest = 0.073,
         periods = list(list(month = 1, day = 15), list(month = 7, day = 15)),
         maturity_date = as.Date("1868-07-15"),
         issue_date = as.Date("1865-07-15"))


#'
#' ## Ten-Forties of 1864
#'
#' Issued under the act of March 3, 1864.
#' Paid interest of 5 percent
#'
#' - Noll, Vol. 6, page 178. <http://www.franklinnoll.com/Vol_6.pdf#page=178>
#' - DeKnight, 93. 
#' - Bayley, 164. 
#' - Annual Report of the Secretary of the Treasury, 1864: 80. 
#' - Annual Report of the Secretary of the Treasury, 1868: 262. 
#' - Annual Report of the Secretary of the Treasury, 1869: 296.
#' 
for (year in 10:40) {
  yyyy <- 1864 + year
  bondname <- sprintf("us_ten_forty_%d0301_call", yyyy)
    coupons[[bondname]] <-
        list(cashflows = generate_cashflow_2(as.Date("1864-3-1"), year * 2, 2.5),
             interest = 0.05,
             periods = list(list(month = 3, day = 1), list(month = 9, day = 1)),
             maturity_date = as.Date(sprintf("%d-3-1", yyyy)),
             issue_date = as.Date("1864-3-1"),
             call = (year < 40))

}

#'
#' ## Five Twenties of 1862
#'
#' 1st possible redemption is 5 years. Interest = 6
#'
#' - Interest and principal payable in specie. George Harrington, "Payment of the Five Twenties in Gold", [Banker's Magazine](http://books.google.com/books?id=D14mAQAAIAAJ&pa=10).
#' 
for (year in 5:20) {
  yyyy <- 1862 + year
  bondname <- sprintf("us_five_twenty_1862_%d0501_call", yyyy)
    coupons[[bondname]] <-
        list(cashflows = generate_cashflow_2(as.Date("1862-5-1"), year * 2, 3),
             interest = 0.06,
             periods = list(list(month = 5, day = 1), list(month = 11, day = 1)),
             call = (year < 20),
             maturity_date = as.Date(sprintf("%d-05-01", yyyy)),
             issue_date = as.Date("1862-5-1"))
}

#'
#' ## Five Twenties of 1864
#' 
for (year in 5:20) {
  yyyy <- 1864 + year
  bondname <- sprintf("us_five_twenty_1864_%d1101_call", yyyy)
  coupons[[bondname]] <-
    list(cashflows = generate_cashflow_2(as.Date("1864-11-1"), year * 2, 3),
         interest = 0.06,
         periods = list(list(month = 5, day = 1), list(month = 11, day = 1)),
         maturity_date = as.Date(sprintf("%d-05-01", yyyy)),
         issue_date = as.Date("1864-11-1"))
}

#'
#' # State Bonds (NYC)
#' 

#'
#' ## California 1870
#'
#' :interest: 7
#' :payable: Jan, Jul

coupons$california_seven_18700701 <-
    list(cashflows = generate_cashflow_1(as.Date("1870-7-1"), 35 * 2, 7 / 2),
         interest = 0.07,
         maturity_date = as.Date("1870-7-1"),
         issue_date = NA,
         periods = list(list(month = 1, day = 1), list(month = 7, day = 1)))

#'
#' ## California 1877
#' 
#'
#' :interest: 7
#' :payable: Jan, Jul

coupons$california_seven_18770701 <-
    list(cashflows = generate_cashflow_1(as.Date("1877-7-1"), 35 * 2, 7 / 2),
         interest = 0.07,
         maturity_date = as.Date("1877-7-1"),
         issue_date = NA,
         periods = list(list(month = 1, day = 1), list(month = 7, day = 1)))

#'
#' ## Ohio 1874
#'
#' :interest: 6
#' :Payable: Jan, Jul
#' :Redeemable: 1874

coupons$ohio_six_18740701 <-
    list(cashflows = generate_cashflow_1(as.Date("1874-7-1"), 35 * 2, 3),
         interest = 0.06,
         maturity_date = as.Date("1874-7-1"),
         issue_date = NA,
         periods = list(list(month = 1, day = 1), list(month = 7, day = 1)))

#'
#' ## Ohio 1886
#'
#' :interest: 6 per cent
#' :Payable: Jan, Jul
#' :Redeemable: 1886

coupons$ohio_six_18860701 <-
    list(cashflows = generate_cashflow_1(as.Date("1886-7-1"), 35 * 2, 3),
         interest = 0.06,
         maturity_date = as.Date("1886-7-1"),
         issue_date = NA,
         periods = list(list(month = 1, day = 1), list(month = 7, day = 1)))

#'
#' ## Kentucky 6's
#' 
#' :interest: 6
#' :Payable: Jan, Jul
#' :Redeemable: 1869-1872

for (year in 1869:1872) {
    bondname <- sprintf("kentucky_six_%d0701", year)
    coupons[[bondname]] <-
        list(cashflows = generate_cashflow_1(as.Date(sprintf("%d-7-1", year)), 35 * 2, 3),
             maturity_date = as.Date(sprintf("%d-7-1", year)),
             issue_date = NA,
             interest = 0.06,
             periods = list(list(month = 1, day = 1), list(month = 7, day = 1)))
}

#'
#'
#' ## Louisiana
#'
#' :interest: 6
#' :Payable: Jan, Jul
#' :redeemable: unknown. Calculate for entire support of state bond redemption dates.
#'
#' Assume issue_date 35 years.

for (year in 1869:1892) {
    bondname <- sprintf("louisiana_six_%d0701", year)
    coupons[[bondname]] <-
        list(cashflows = generate_cashflow_1(as.Date(sprintf("%d-7-1", year)), 40 * 2, 3),
             maturity_date = as.Date(sprintf("%d-7-1", year)),
             issue_date = NA,
             interest = 0.06,
             periods = list(list(month = 1, day = 1), list(month = 7, day = 1)))
}

#'
#' ## Missouri
#'
#' interest 6 per cent
#' redeemable in 1872
#' payable jan, jul

coupons$missouri_six_18720701 <-
    list(cashflows = generate_cashflow_1(as.Date("1872-7-1"), 35 * 2, 3),
         maturity_date = as.Date("1872-7-1"),
         issue_date = NA,
         interest = 0.06,
         periods = list(list(month = 1, day = 1), list(month = 7, day = 1)))

#'
#' ## North Carolina
#'
#' interest 6 per cent
#' payable jan, jul
#' redeemable in 1873

coupons$north_carolina_six_18730701 <-
    list(cashflows = generate_cashflow_1(as.Date("1873-7-1"), 35 * 2, 3),
         maturity_date = as.Date("1873-7-1"),
         issue_date = NA,
         interest = 0.06,
         periods = list(list(month = 1, day = 1), list(month = 7, day = 1)))

#'
#' ## Pennsylvania
#'
#' :interest: 6 per cent
#' :redeemable: 1873
#' :payable: Feb, Aug

coupons$pennsylvania_six_18730801 <-
    list(cashflows = generate_cashflow_1(as.Date("1871-8-1"), 35 * 2, 3),
         maturity_date = as.Date("1873-8-1"),
         issue_date = NA,
         interest = 0.06,
         periods = list(list(month = 2, day = 1), list(month = 8, day = 1)))

#'
#' ## Virginia
#'
#' :Interest: 6
#' :payable: Jan, Jul
#' :redeemable: 1885-1890

for (year in 1885:1890) {
    bondname <- sprintf("virginia_six_%d0701", year)
    coupons[[bondname]] <-
        list(cashflows = generate_cashflow_1(as.Date(sprintf("%d-7-1", year)), 40 * 2, 3),
             maturity_date = as.Date(sprintf("%s-8-1", year)),
             issue_date = NA,
             interest = 0.06,
             periods = list(list(month = 1, day = 1), list(month = 7, day = 1)))
}

#'
#' ## Tennessee
#'
#' :Interest: 6
#' :payable: Jan, Jul
#' :redeemable: 1885-1892

for (year in 1885:1892) {
    bondname <- sprintf("tennessee_six_%d0701", year)
    coupons[[bondname]] <-
        list(cashflows = generate_cashflow_1(as.Date(sprintf("%d-7-1", year)), 40 * 2, 3),
             maturity_date = as.Date(sprintf("%s-7-1", year)),
             issue_date = NA,
             interest = 0.06,
             periods = list(list(month = 1, day = 1), list(month = 7, day = 1)))
}

#'
#' ## Indiana 5
#'
#' :interest: 5
#' :payable: Jan, Jul
#' :redeemable: unknown. Use 1869-1892

for (year in 1869:1892) {
    bondname <- sprintf("indiana_five_%d0701", year)
    coupons[[bondname]] <-
        list(cashflows = generate_cashflow_1(as.Date(sprintf("%d-7-1", year)), 40 * 2, 2.5),
             maturity_date = as.Date(sprintf("%s-7-1", year)),
             issue_date = NA,
             interest = 0.05,
             periods = list(list(month = 1, day = 1), list(month = 7, day = 1)))
}

#'
#' ## Indiana 6's
#'
#' :interest: 6
#' :payable: May, Nov
#' :redeemable: 1881
#' :issued: known to be issued in 1861. Mentioned in June.

coupons$indiana_six_18810501 <-
    list(cashflows = generate_cashflow_1(as.Date("1881-11-1"), 30 * 2, 3),
         maturity_date = as.Date(sprintf("%s-11-01", year)),
         issue_date = NA,
         interest = 0.06,
         periods = list(list(month = 5, day = 1), list(month = 11, day = 1)))

#'
#' ## Georgia
#'
#' :interest: 6
#' :payable: Jan, Jul
#' :redeemable: 1872

coupons$georgia_six_18720701 <-
    list(cashflows = generate_cashflow_1(as.Date("1872-7-1"), 35 * 2, 3),
         maturity_date = as.Date("1872-07-1"),
         issue_date = NA,
         interest = 0.06,
         periods = list(list(month = 1, day = 1), list(month = 7, day = 1)))


#'
#' # Confederate Bonds
#' 

## #' ## Confederate 5 million loan
## #'
## #' Act of Feb 28, 1861, Specie Loan
## #'
## #' :authorized: unlimited
## #' :maturity: 10 years (9-1-1871), callable after 5 years
## #' :interest: 8 per cent, coupons receivable for export dues
## #' :total issued: 15 million
## #'

## for (year in 5:10) {
##     bondname <- sprintf("confed_15mn_%d", year)
##     coupons[[bondname]] <-
##         list(cashflows = generate_cashflow_2(as.Date("1861-9-1"), year * 2, 4),
##              interest = 0.08,
##              periods = list(list(month = 3, day = 1), list(month = 9, day = 1)),
##              issued = as.Date("1861-9-1"))
## }

## #' Confederate 100 million
## #' ------------------------
## #'
## #' Act of Aug 19, 1861
## #' 
## #' :authorized: 100 mn
## #' :maturity: various dates, Jul 1, 1864 (3 yrs)-Jul 1, 1881 (20 yrs)
## #' :interest: 8 per cent in liue of authorized by Act of May 16, 1861
## #' :total issued:  $99,570,550

## for (year in 3:20) {
##     bondname <- sprintf("confed_100mn_%d", year)
##     coupons[[bondname]] <-
##         list(cashflows = generate_cashflow_2(as.Date("1861-7-1"), year * 2, 4),
##              interest = 0.08,
##              periods = list(list(month = 1, day = 1), list(month = 7, day = 1)),
##              issued = as.Date("1861-7-1"))
## }

## #' Confederate Sevens
## #' -------------------
## #'
## #' Only issue that had 7 per cent rate was by the Feb 20, 1863 Funding Loan.
## #'
## #' Just calculate using Jan / Jul interest payments even though they seem to depend
## #' on the issue date of the bond.
## #'
## #' :authorized:
## #' :maturity: 5 years, but Congress could defer payment up to 30 yrs at smae rate of interest.
## #' :interest: 8 percent bonds sold until April 21, 1863 and 7 per cent bonds until Aug 1, 1863.
## #' :issued: 8 percent: 96 million , 7 percent 76 million.

## for (year in 5:30) {
##     bondname <- sprintf("confed_7_%d", year)
##     coupons[[bondname]] <-
##         list(cashflows = generate_cashflow_2(as.Date("1863-7-1"), year * 2, 4),
##              interest = 0.08,
##              periods = list(list(month = 1, day = 1), list(month = 7, day = 1)),
##              issued = as.Date("1863-4-21"))
## }

#' Confederate 7'30s
#' --------------------
#'
#' Act of April 17, 1862
#'
#' :authorized: up to 165 million in lieu of April 12 (18) 1862 bonds
#' :denomination: 100 or higher
#' :maturity: 6 months after peace
#' :interest rate: 2 cents per day per 100
#' :total issued: 128,241,400
## No fixed maturity, so cannot calculate


#' Richmond
#' ------------
#'
#' Assume date of 1879 as in Davis and Pecquet (1990)

#' Petersburg
#' -------------
#'
#' Don't have a date, so assume the same date at Richmond.

cat(toJSON(coupons, asIs = FALSE), "\n", file = outfile)
