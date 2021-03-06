#' A function for simple projections of tables of Australian Taxation Office tax returns.
#' 
#' @param sample_file A sample file, most likely the 2012-13 sample file. It is intended that to be the most recent.
#' @param h An integer. How many years should the sample file be projected?
#' @param fy.year.of.sample.file The financial year of \code{sample_file}.
#' @param WEIGHT The sample weight for the sample file. (So a 2\% file has \code{WEIGHT} = 50.)
#' @param excl_vars A character vector of column names in \code{sample_file} that should not be inflated. Columns not present in the 2013-14 sample file are not inflated and nor are the columns \code{Ind}, \code{Gender}, \code{age_range}, \code{Occ_code}, \code{Partner_status}, \code{Region}, \code{Lodgment_method}, and \code{PHI_Ind}.
#' @param forecast.dots A list containing parameters to be passed to \code{generic_inflator}.
#' @param wage.series See \code{\link{wage_inflator}}.
#' @param lf.series See \code{\link{lf_inflator_fy}}.
#' @param .recalculate.inflators Should \code{generic_inflator()} or \code{CG_inflator} be called to project the other variables? Adds time.
#' @param .copyDT Should a \code{copy()} of \code{sample_file} be made? If set to FALSE, will update \code{sample_file}. 
#' @return A sample file of the same number of rows as \code{sample_file} with inflated values (including WEIGHT).
#' @details We recommend you use \code{sample_file_1213}, rather than \code{sample_file_1314}, unless you need the superannuation variables, 
#' as the latter suggests lower-than-recorded tax collections. 
#' @examples 
#' if (requireNamespace("taxstats", quietly = TRUE) && requireNamespace("data.table", quietly = TRUE)){
#'   library(taxstats)
#'   library(data.table)
#'   sample_file <- copy(sample_file_1314)
#'   sample_file_1617 <- project(sample_file, h = 3L)  # to "2016-17"
#' }
#' @import data.table
#' @export

project <- function(sample_file, 
                    h = 0L, 
                    fy.year.of.sample.file = "2013-14", 
                    WEIGHT = 50L, 
                    excl_vars, 
                    forecast.dots = list(estimator = "mean", pred_interval = 80), 
                    wage.series = NULL,
                    lf.series = NULL,
                    .recalculate.inflators = FALSE, 
                    .copyDT = TRUE){
  stopifnot(is.integer(h), h >= 0L, is.data.table(sample_file))
  if (.copyDT){
    sample_file <- copy(sample_file)
  }
  
  sample_file[, "WEIGHT" := list(WEIGHT)]
  
  if (h == 0){
    return(sample_file)
  } else {
    current.fy <- fy.year.of.sample.file
    to.fy <- yr2fy(fy2yr(current.fy) + h)
    
    if (is.null(wage.series)){
      wage.inflator <- wage_inflator(1, from_fy = current.fy, to_fy = to.fy)
    } else {
      wage.inflator <- wage_inflator(1, from_fy = current.fy, to_fy = to.fy, 
                                     forecast.series = "custom", wage.series = wage.series)
    }
    
    if (is.null(lf.series)){
      lf.inflator <- lf_inflator_fy(from_fy = current.fy, to_fy = to.fy)
    } else {
      lf.inflator <- lf_inflator_fy(from_fy = current.fy, to_fy = to.fy, 
                                    forecast.series = "custom", lf.series = lf.series)
    }
    
    
    cpi.inflator <- cpi_inflator(1, from_fy = current.fy, to_fy = to.fy)
    if (.recalculate.inflators){
      CG.inflator <- CG_inflator(1, from_fy = current.fy, to_fy = to.fy)
    } else {
      if (current.fy %notin% c("2012-13", "2013-14")){
        stop("Precalculated inflators only available when projecting from 2012-13 or 2013-14.")
      } else {
        cg_inflators <- 
          switch(current.fy, 
                 "2012-13" = cg_inflators_1213, 
                 "2013-14" = cg_inflators_1314)
        stopifnot("forecast.series" %in% names(cg_inflators))
        forecast.series <- NULL 
        CG.inflator <- cg_inflators[fy_year == to.fy & forecast.series == forecast.dots$estimator][["cg_inflator"]]
      } 
    }
    
    col.names <- names(sample_file)
    
    # Differential uprating not available for years outside:
    stopifnot(fy.year.of.sample.file %in% yr2fy(2004:2014))
    diff.uprate.wagey.cols <- "Sw_amt"
    
    wagey.cols <- c(
                    "Alow_ben_amt",
                    "ETP_txbl_amt",
                    "Rptbl_Empr_spr_cont_amt", 
                    "Non_emp_spr_amt", 
                    "MCS_Emplr_Contr", 
                    "MCS_Prsnl_Contr", 
                    "MCS_Othr_Contr")
    
    super.bal.col <- c("MCS_Ttl_Acnt_Bal")
    
    lfy.cols <- c("WEIGHT")
    
    cpiy.cols <- c(grep("WRE", col.names, value = TRUE), # work-related expenses
                   "Cost_tax_affairs_amt",
                   "Other_Ded_amt")
    
    derived.cols <- c("Net_rent_amt",
                      "Net_PP_BI_amt",
                      "Net_NPP_BI_amt",
                      "Tot_inc_amt",
                      "Tot_ded_amt",
                      "Taxable_Income")
    
    CGTy.cols <- c("Net_CG_amt", "Tot_CY_CG_amt")
    
    # names(taxstats::sample_file_1314)
    alien.cols <- col.names[!col.names %in% c("Ind", "Gender", "age_range", "Occ_code", "Partner_status", 
                                              "Region", "Lodgment_method", "PHI_Ind", "Sw_amt", "Alow_ben_amt", 
                                              "ETP_txbl_amt", "Grs_int_amt", "Aust_govt_pnsn_allw_amt", "Unfranked_Div_amt", 
                                              "Frk_Div_amt", "Dividends_franking_cr_amt", "Net_rent_amt", "Gross_rent_amt", 
                                              "Other_rent_ded_amt", "Rent_int_ded_amt", "Rent_cap_wks_amt", 
                                              "Net_farm_management_amt", "Net_PP_BI_amt", "Net_NPP_BI_amt", 
                                              "Total_PP_BI_amt", "Total_NPP_BI_amt", "Total_PP_BE_amt", "Total_NPP_BE_amt", 
                                              "Net_CG_amt", "Tot_CY_CG_amt", "Net_PT_PP_dsn", "Net_PT_NPP_dsn", 
                                              "Taxed_othr_pnsn_amt", "Untaxed_othr_pnsn_amt", "Other_foreign_inc_amt", 
                                              "Other_inc_amt", "Tot_inc_amt", "WRE_car_amt", "WRE_trvl_amt", 
                                              "WRE_uniform_amt", "WRE_self_amt", "WRE_other_amt", "Div_Ded_amt", 
                                              "Intrst_Ded_amt", "Gift_amt", "Non_emp_spr_amt", "Cost_tax_affairs_amt", 
                                              "Other_Ded_amt", "Tot_ded_amt", "PP_loss_claimed", "NPP_loss_claimed", 
                                              "Rep_frng_ben_amt", "Med_Exp_TO_amt", "Asbl_forgn_source_incm_amt", 
                                              "Spouse_adjusted_taxable_inc", "Net_fincl_invstmt_lss_amt", "Rptbl_Empr_spr_cont_amt", 
                                              "Cr_PAYG_ITI_amt", "TFN_amts_wheld_gr_intst_amt", "TFN_amts_wheld_divs_amt", 
                                              "Hrs_to_prepare_BPI_cnt", "Taxable_Income", "Help_debt", "MCS_Emplr_Contr", 
                                              "MCS_Prsnl_Contr", "MCS_Othr_Contr", "MCS_Ttl_Acnt_Bal")]
    Not.Inflated <- c("Ind", 
                      "Gender",
                      "age_range", 
                      "Occ_code", 
                      "Partner_status", 
                      "Region", 
                      "Lodgment_method", 
                      "PHI_Ind", 
                      alien.cols)
    
    if(!missing(excl_vars)){
      Not.Inflated <- c(Not.Inflated, excl_vars)
    }
    
    SetDiff <- function(...) Reduce(setdiff, list(...), right = FALSE)
    
    generic.cols <- SetDiff(col.names, 
                            diff.uprate.wagey.cols, wagey.cols, super.bal.col, lfy.cols, cpiy.cols, derived.cols, Not.Inflated)
    
    if (.recalculate.inflators){
      generic.inflators <- 
        generic_inflator(vars = generic.cols, h = h, fy.year.of.sample.file = fy.year.of.sample.file, 
                         estimator = forecast.dots$estimator, pred_interval = forecast.dots$pred_interval)
    } else {
      switch(current.fy, 
             "2012-13" = generic.inflators <- dplyr::filter(generic_inflators_1213, fy_year == to.fy), 
             "2013-14" = generic.inflators <- dplyr::filter(generic_inflators_1314, fy_year == to.fy), 
             stop("Precalculated inflators only available when projecting from 2012-13 or 2013-14.")
      )
      generic.inflators <- as.data.table(generic.inflators)
    }
    
    ## Inflate:
    # make numeric to avoid overflow
    numeric.cols <- names(sample_file)[vapply(sample_file, is.numeric, TRUE)]
    for (j in which(col.names %in% numeric.cols))
      set(sample_file, j = j, value = as.numeric(sample_file[[j]]))
    
    
    # Differential uprating:
    for (j in which(col.names %in% diff.uprate.wagey.cols)){
      if (is.null(wage.series)){
        set(sample_file, j = j, value = differentially_uprate_wage(sample_file[[j]], from_fy = current.fy, to_fy = to.fy))
      } else {
        set(sample_file, j = j, value = differentially_uprate_wage(sample_file[[j]], from_fy = current.fy, to_fy = to.fy, 
                                                                   forecast.series = "custom", wage.series = wage.series))
      }
    }
    
    for (j in which(col.names %in% wagey.cols))
      set(sample_file, j = j, value = wage.inflator * sample_file[[j]])
    
    for (j in which(col.names %in% lfy.cols))
      set(sample_file, j = j, value = lf.inflator * sample_file[[j]])
    
    for (j in which(col.names %in% cpiy.cols))
      set(sample_file, j = j, value = cpi.inflator * sample_file[[j]])
    
    for (j in which(col.names %in% CGTy.cols))
      set(sample_file, j = j, value = CG.inflator * sample_file[[j]])
    
    for (j in which(col.names %in% generic.cols)){
      stopifnot("variable" %in% names(generic.inflators))  ## super safe
      nom <- col.names[j]
      set(sample_file, 
          j = j, 
          value = generic.inflators[variable == nom]$inflator * sample_file[[j]])
    }
    
    for (j in which(col.names %in% super.bal.col)){
      set(sample_file, j = j, value = (1.05 ^ h) * sample_file[[j]])
    }
    
    sample_file %>%
      .[, Net_rent_amt := Gross_rent_amt - Other_rent_ded_amt - Rent_int_ded_amt - Rent_cap_wks_amt] %>%
      .[, Net_PP_BI_amt := Total_PP_BI_amt - Total_PP_BE_amt] %>%
      .[, Net_NPP_BI_amt := Total_NPP_BI_amt - Total_NPP_BE_amt] %>%
      .[, Tot_inc_amt := .add(Sw_amt,
                              Alow_ben_amt,
                              ETP_txbl_amt,
                              Grs_int_amt,
                              Aust_govt_pnsn_allw_amt,
                              Unfranked_Div_amt,
                              Frk_Div_amt,
                              Dividends_franking_cr_amt,
                              Net_rent_amt,
                              Net_farm_management_amt,
                              Net_PP_BI_amt,  ## Need to check exactly how this maps.
                              Net_NPP_BI_amt,
                              Net_CG_amt,  ## We cannot express this cleanly in terms of Tot_CG
                              Net_PT_PP_dsn,
                              Net_PT_NPP_dsn,
                              Taxed_othr_pnsn_amt,
                              Untaxed_othr_pnsn_amt,
                              Other_foreign_inc_amt,
                              Other_inc_amt)] %>%
      .[, Tot_ded_amt := .add(WRE_car_amt,
                              WRE_trvl_amt,
                              WRE_uniform_amt,
                              WRE_self_amt,
                              WRE_other_amt,
                              Div_Ded_amt,
                              Intrst_Ded_amt,
                              Gift_amt,
                              Non_emp_spr_amt,
                              Cost_tax_affairs_amt,
                              Other_Ded_amt)] %>%
      .[, Taxable_Income := pmaxC(Tot_inc_amt - Tot_ded_amt - PP_loss_claimed - NPP_loss_claimed, 0)] 
    
  } 
}