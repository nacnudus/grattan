% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lf_inflator.R
\name{lf_inflator}
\alias{lf_inflator}
\alias{lf_inflator_fy}
\alias{lf_inflator}
\title{Labour force inflators}
\source{
ABS Cat 6202.0 \url{http://www.abs.gov.au/ausstats/abs@.nsf/mf/6202.0?OpenDocument}.
}
\usage{
lf_inflator_fy(labour_force = 1, from_fy = "2012-13", to_fy,
  useABSConnection = FALSE, allow.projection = TRUE, use.month = 1L,
  forecast.series = c("mean", "upper", "lower", "custom"),
  forecast.level = 95, lf.series = NULL)

lf_inflator(labour_force = 1, from_date = "2013-06-30", to_date,
  useABSConnection = FALSE)
}
\arguments{
\item{labour_force}{A numeric vector.}

\item{from_fy}{Financial year of \code{labour_force}.}

\item{to_fy}{Financial year for which the labour force is predicted.}

\item{useABSConnection}{Should the function connect with ABS.Stat via an SDMX connection? If \code{FALSE} (the default), a pre-prepared index table is used. This is much faster and more reliable (in terms of errors), though of course relies on the package maintainer to keep the tables up-to-date. The internal data was updated on 2017-08-16.}

\item{allow.projection}{Logical. Should projections be allowed?}

\item{use.month}{An integer (corresponding to the output of \code{data.table::month}) representing the month of the series used for the inflation.}

\item{forecast.series}{Whether to use the forecast mean, or the upper or lower boundaries of the prediction intervals.}

\item{forecast.level}{The prediction interval to be used if \code{forecast.series} is \code{upper} or \code{lower}.}

\item{lf.series}{If \code{forecast.series = 'custom'}, a \code{data.table} with two variables, \code{fy_year} and \code{r}. 
The variable \code{fy_year} consists of all financial years between the last financial year in the (known) labour force series and \code{to_fy} \strong{inclusive}.
The variable \code{r} consists of rates of labour force growth assumed in each \code{fy_year}, which must be 1 in the first year (to connect with the original labour force series).}

\item{from_date}{The date of \code{labour_force}.}

\item{to_date}{Dates as a character vector.}
}
\value{
The relative labour force between \code{to_date} and \code{for_date} or \code{to_fy} and \code{from_fy}, multiplied by \code{labour_force}.
}
\description{
Labour force inflators
}
\details{
\code{lf_inflator} is used on dates. The underlying data series is available every month.
}
\examples{
lf_inflator_fy(labour_force = 1, from_fy = "2012-13", to_fy = "2013-14")
\dontrun{
lf_inflator(labour_force = 1, from_date = "2013-06-30", to_date = "2014-06-30")
}
}
\author{
Hugh Parsonage and Tim Cameron
}
