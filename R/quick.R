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
#' quick("MLR", limit = 1) |> str()
#' quick("enterprise", limit = 1) |> str()
#' quick("demographics", limit = 1) |> str()
#'
#' quick("suppliers", limit = 1) |> str()
#' quick("PDC", limit = 1) |> str()
#' quick("MIPS", limit = 1) |> str()
#' quick("LTCH", limit = 1) |> str()
#' quick("IRF", limit = 1) |> str()
#' quick("SPICE", limit = 1) |> str()
#' quick("CAHPS_hospice", limit = 1) |> str()
#' quick("HHVBP", limit = 1) |> str()
#' quick("HHCAHPS", limit = 1) |> str()
#' quick("HHCAHPS", limit = 1) |> str()
#' quick("SNF_VBP", limit = 1) |> str()
#' quick("SNF_quality", limit = 1) |> str()
#' quick("NH_pro", limit = 1) |> str()
#'
#' quick("summary", limit = 1) |> str()
#' quick("profile", limit = 1) |> str()
#'
#' quick("HHA", limit = 1) |> str()
#' quick("hospice", limit = 1) |> str()
#' quick("hospital", limit = 1) |> str()
#' quick("RHC", limit = 1) |> str()
#' quick("FQHC", limit = 1) |> str()
#' quick("pending", limit = 1) |> str()
#' quick("reassignment", limit = 1) |> str()
#' quick("SNF", limit = 1) |> str()
#'
#' quick("contact", limit = 1) |> str()
#' quick("crosswalk", limit = 1) |> str()
#' quick("CARE_dialysis", limit = 1) |> str()
#' quick("enrollees", limit = 1) |> str()
#' quick("facilities", limit = 1) |> str()
#' quick("IQIES", limit = 1) |> str()
#' quick("laboratories", limit = 1) |> str()
#' quick("long_term", limit = 1) |> str()
#' quick("opt_out", limit = 1) |> str()
#' quick("order_refer", limit = 1) |> str()
#' quick("RBCS", limit = 1) |> str()
#' quick("transparency", limit = 1) |> str()
#' @autoglobal
#' @export
quick <- function(x, offset = 0L, limit = 10000L, call = caller_env()) {
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
    transparency      = quick_(careMain(x), offset = offset, limit = limit),
    HHA               = ,
    hospice           = ,
    hospital          = ,
    RHC               = ,
    FQHC              = ,
    pending           = ,
    reassignment      = ,
    SNF               = quick_(careGroup(x), offset = offset, limit = limit),
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
    SUMM_nation_group = quick_(openMain(x), offset = offset, limit = limit),
    profile           = ,
    summary           = quick_(openGroup(x), offset = offset, limit = limit),
    MLR               = ,
    enterprise        = quick_(caid_endpoint(x), offset = offset, limit = limit),
    demographics      = quick_(caid_group(x), offset = offset, limit = limit),

    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
}
