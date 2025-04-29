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
#' # MEDICAID
#' quick("MLR") |> str()
#' quick("enterprise") |> str()
#' quick("demographics") |> str()
#'
#'
#' # PROVIDER
#' quick("suppliers") |> str()
#' quick("PDC") |> str()
#' quick("MIPS") |> str()
#' quick("LTCH") |> str()
#' quick("IRF") |> str()
#' quick("SPICE") |> str()
#' quick("CAHPS_hospice") |> str()
#' quick("HHVBP") |> str()
#' quick("HHCAHPS") |> str()
#' quick("HHCAHPS") |> str()
#' quick("SNF_VBP") |> str()
#' quick("SNF_quality") |> str()
#' quick("NH_pro") |> str()
#'
#' # OPEN PAYMENTS
#' quick("summary") |> str()
#' quick("profile") |> str()
#'
#' # MEDICARE
#' quick("HHA") |> str()
#' quick("hospice") |> str()
#' quick("hospital") |> str()
#' quick("RHC") |> str()
#' quick("FQHC") |> str()
#' quick("pending") |> str()
#' quick("reassignment") |> str()
#' quick("SNF") |> str()
#'
#' quick("contact") |> str()
#' quick("crosswalk") |> str()
#' quick("CARE_dialysis") |> str()
#' quick("enrollees") |> str()
#' quick("facilities") |> str()
#' quick("IQIES") |> str()
#' quick("laboratories") |> str()
#' quick("long_term") |> str()
#' quick("opt_out") |> str()
#' quick("order_refer") |> str()
#' quick("RBCS") |> str()
#' quick("transparency") |> str()
#' @autoglobal
#' @export
quick <- function(x, offset = 0L, limit = 1L, call = caller_env()) {
  switch(
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
    transparency      = quick_(care_endpoint(x), offset = offset, limit = limit),
    HHA               = ,
    hospice           = ,
    hospital          = ,
    RHC               = ,
    FQHC              = ,
    pending           = ,
    reassignment      = ,
    SNF               = quick_(care_group(x), offset = offset, limit = limit),
    suppliers         = quick_(pro_endpoint(x), offset = offset, limit = limit),
    PDC               = ,
    MIPS              = ,
    LTCH              = ,
    IRF               = ,
    SPICE             = ,
    CAHPS_hospice     = ,
    HHVBP             = ,
    HHC               = ,
    HHCAHPS           = ,
    SNF_VBP           = ,
    SNF_quality       = ,
    NH_pro            = quick_(pro_group(x), offset = offset, limit = limit),
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
    SUMM_nation_group = quick_(open_endpoint(x), offset = offset, limit = limit),
    profile           = ,
    summary           = quick_(open_group(x), offset = offset, limit = limit),
    MLR               = ,
    enterprise        = quick_(caid_endpoint(x), offset = offset, limit = limit),
    demographics      = quick_(caid_group(x), offset = offset, limit = limit),

    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
}
