#' Quickly access CMS data
#'
#' `quick()` is a convenience function to quickly access various CMS data endpoints.
#' It provides a simple way to retrieve data from different CMS programs and categories.
#'
#' @param x A string representing the CMS data endpoint or category.
#' @param offset An integer representing the offset for pagination. Default is 0.
#' @param limit An integer representing the maximum number of records to retrieve. Default is 10,000.
#' @param call The calling environment. Default is the current environment.
#'
#' @returns A data frame or object containing the requested CMS data.
#'
#' @examples
#' quick("MLR") |> str()
#' @autoglobal
#' @export
quick <- function(x, offset = 0L, limit = 1L, call = caller_env()) {
  ob <- switch(
    x,
    contact           = ,
    crosswalk         = ,
    CARE_dialysis     = ,
    enrollees         = ,
    facilities        = ,
    IQIES             = ,
    laboratories      = ,
    long_term         = ,
    opt_out           = ,
    order_refer       = ,
    RBCS              = ,
    transparency      = care_endpoint(x),
    HHA               = ,
    hospice           = ,
    hospital          = ,
    RHC               = ,
    FQHC              = ,
    pending           = ,
    reassignment      = ,
    SNF               = care_group(x),
    PSI90_6digit      = ,
    joint_replace     = ,
    Data_Updates      = ,
    Footnote_Xwalk    = ,
    Measure_Dates     = ,
    Hospital_Maternal = ,
    Hospital_Outcomes = ,
    Hospital_General  = ,
    Hospital_PI       = ,
    suppliers         = pro_endpoint(x),
    CAHPS_SPICE       = ,
    CAHPS_HHC         = ,
    CAHPS_ICH         = ,
    CAHPS_OAS         = ,
    HCAHPS            = ,
    MIPS              = ,
    PDC               = ,
    LTCH              = ,
    IPF               = ,
    IRF               = ,
    SPICE             = ,
    HVBP              = ,
    HHVBP             = ,
    HHC               = ,
    SNF_VBP           = ,
    SNF_quality       = ,
    NURSING_HOMES     = ,
    COMP_DEATH        = ,
    PCH_COMP          = ,
    PCH_PALL          = ,
    PCH_HCAHPS        = ,
    ASC_quality       = ,
    EQUI              = ,
    HAI               = ,
    DIAL              = ,
    ESRD              = ,
    MSPB              = ,
    OUT_img           = ,
    Timely            = ,
    UNPLAN            = ,
    VHA               = ,
    CHANGES           = ,
    VOC               = ,
    REDUCT            = pro_group(x),
    PROF_covered      = ,
    PROF_physician    = ,
    PROF_information  = ,
    PROF_mapping      = ,
    PROF_entity       = ,
    PROF_teaching     = ,
    SUMM_dashboard    = ,
    SUMM_state_all    = ,
    SUMM_state_group  = ,
    SUMM_nation_all   = ,
    SUMM_nation_group = open_endpoint(x),
    profile           = ,
    summary           = open_group(x),
    MLR               = ,
    enterprise        = caid_endpoint(x),
    demographics      = caid_group(x),
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
  quick_(ob, offset = offset, limit = limit)
}
