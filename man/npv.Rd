% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/financialfunctions.R
\name{npv}
\alias{npv}
\alias{irr}
\alias{fv}
\alias{pv}
\alias{pmt}
\alias{\url{http://cvs.moodle.org/contrib/patches/question_calculated_extended/calculated/packages/financial/financial_class.php?view=co}}
\alias{irr}
\alias{fv}
\alias{pv}
\alias{pmt}
\title{Financial functions}
\usage{
npv(rate, values)

irr(x, start = 0.1)

fv(rate, nper, pmt, pv = 0, type = 0)

pv(rate, nper, pmt, fv = 0, type = 0)

pmt(rate, nper, pv, fv = 0, type = 0)
}
\arguments{
\item{rate}{Discount or interest rate.}

\item{values}{Income stream.}

\item{x}{Cash flow.}

\item{start}{Initial guess to start the iterative process.}

\item{nper}{Number of periods}

\item{pmt}{Payments.}

\item{pv}{Present value.}

\item{type}{Factor.}

\item{fv}{Future value.}
}
\description{
Financial functions from Excel. These functions are equivalent to the Excel functions of the same name (in uppercase).
}
\examples{
npv(0.07, c(1, 2))
irr(x = c(1, -1), start = 0.1)
fv(0.04, 7, 1, pv = 0.0, type = 0)
pv(rate = 0.08, nper = 7, pmt = 1, fv = 0.0, type = 0)
pmt(rate = 0.025, nper = 7, pv = 0, fv = 0.0, type = 0)
}
\author{
Enrique Garcia M. \email{egarcia@egm.as}

Karsten W. \email{k.weinert@gmx.net}
}
