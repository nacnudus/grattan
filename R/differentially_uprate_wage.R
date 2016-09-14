#' Differential uprating
#' @description Apply differential uprating to projections of the \code{Sw_amt} variable.
#' @author Hugh Parsonage and William Young
#' @param wage A numeric vector to be uprated.
#' @param from_fy The financial year of the origin of the wage, which must be a financial year of an available sample file. (Not after 2013-14.)
#' @param to_fy The target of the uprating. Passed to \code{\link{wage_inflator}}.
#' @export


differentially_uprate_wage <- function(wage = 1, from_fy, to_fy){
  stopifnot(all(from_fy %in% c("2003-04", "2004-05", "2005-06", "2006-07", "2007-08", "2008-09", 
                               "2009-10", "2010-11", "2011-12", "2012-13", "2013-14")))
  input <- 
    data.table(fy.year = from_fy, 
               wage = wage) %>%
    # rolling merge will destroy order
    .[, `_order` := 1:.N] %>%
    setkeyv(cols = c("fy.year", "wage"))
  
  `_order` <- NULL
  
  assertthat::validate_that(assertthat::has_name(differential_sw_uprates, "uprate_factor"))
  uprate_factor <- NULL
  `_out` <- NULL
  
  differential_sw_uprates[salary_by_fy_swtile] %>%
    setnames(old = "min_salary", new = "wage") %>%
    setkeyv(cols = c("fy.year", "wage")) %>%
    .[input, roll = "nearest"] %>%
    .[, `_out` := wage * (uprate_factor * (wage_inflator(from_fy = from_fy, to_fy = to_fy) - 1) + 1)] %>%
    setkeyv("_order") %>%
    .[["_out"]]
}