#' Create Offset Sequence
#'
#' @param rows `<int>` The number of rows found in the dataset
#'
#' @param size `<int>` The size or limit of the offset sequence
#'
#' @returns `<int>` vector, length equal to rows / size, as sequence begins
#'                  at zero. If `rows` is less than or equal to `size`, the
#'                  function simply returns `rows`.
#'
#' @examples
#' offset_sequence(rows = 100, size = 10)
#'
#' offset_sequence(rows = 47984, size = 5000)
#'
#' @autoglobal
#'
#' @export
offset_sequence <- \(rows, size = 5000) {

  if (rows <= size) return(rows)

  0:round(rows / size) * size

}

#' Replace All Fixed Strings
#'
#' @param x `<chr>` vector or string to search in
#'
#' @param p `<chr>` vector of search patterns
#'
#' @param r `<chr>` vector of replacements for matched patterns
#'
#' @param v `<lgl>` default is FALSE; should each occurrence of a pattern in
#'   every string be replaced by a corresponding replacement string?
#'
#' @returns `<chr>` vector with all occurrences of each pattern replaced
#'
#' @examplesIf FALSE
#' replace_fixed(
#'   x,
#'   c(":", "%", "@", "$"),
#'   c("_", "", "", ""))
#'
#' @autoglobal
#'
#' @export
replace_fixed <- \(x, p, r, v = FALSE) {
  stringi::stri_replace_all_fixed(
    str = x,
    pattern = p,
    replacement = r,
    vectorize_all = v)
}

#' ISO 8601 Recurring Time Intervals
#'
#' @param x `<chr>` vector of ISO8601 recurrence rules
#'
#' @returns `<chr>` vector of human-readable recurrence rule descriptions
#'
#' @examples
#' accrualPeriodicity = c(
#'   "R/PT1S",   "R/PT1H",  "R/P1D", "R/P3.5D",
#'   "R/P0.33W", "R/P0.5W", "R/P1W", "R/P2W",
#'   "R/P0.33M", "R/P0.5M", "R/P1M", "R/P2M",
#'   "R/P3M",    "R/P4M",   "R/P6M", "R/P1Y",
#'   "R/P2Y",    "R/P3Y",   "R/P4Y", "R/P10Y")
#'
#' recode_iso8601(accrualPeriodicity)
#'
#' @section References:
#'    * [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601)
#'    * [ISO 8601 Repeating_intervals](https://en.wikipedia.org/wiki/ISO_8601#Repeating_intervals)
#'    * [Recurring Time Intervals](https://sentenz.github.io/convention/convention/iso-8601/#19-recurring-time-intervals)
#'
#' @autoglobal
#'
#' @export
recode_iso8601 <- \(x) {
  kit::nswitch(
    x,
    "R/P10Y",   "Decennially",
    "R/P4Y",    "Quadrennially",
    "R/P1Y",    "Annually",
    "R/P0.5M",  "Twice a Month",
    "R/P2M",    "Bimonthly",
    "R/P0.5W",  "Twice a Week",
    "R/P2W",    "Biweekly",
    "R/P3.5D",  "Semiweekly",
    "R/P1D",    "Daily",
    "R/P6M",    "Semiannually",
    "R/P2Y",    "Biennially",
    "R/P3Y",    "Triennially",
    "R/P0.33W", "Three Times a Week",
    "R/P0.33M", "Three Times a Month",
    "R/PT1S",   "Continuously Updated",
    "R/P1M",    "Monthly",
    "R/P3M",    "Quarterly",
    "R/P4M",    "Three Times a Year",
    "R/P1W",    "Weekly",
    "R/PT1H",   "Hourly",
    default = NA_character_,
    nThread = 4L
  )
}

#' Program Code
#'
#' Primary program related to a data asset,
#' from the [Federal Program Inventory](https://resources.data.gov/schemas/dcat-us/v1.1/FederalProgramInventory_FY13_MachineReadable_091613.csv).
#'
#' @param x `<chr>`  The program code to search for, e.g., `"009:000"`; if
#'   `NULL` (the default), returns all program codes
#'
#' @returns `<tibble>` of search results
#'
#' @examples
#' program_code("009:000")
#'
#' head(program_code())
#'
#' @autoglobal
#'
#' @export
program_code <- \(x = NULL) {

  search_in(
    get_pin("programCodes"),
    "programCodePODfmt",
    x)
}

#' Bureau Code
#'
#' Combined Agency and Bureau Code,
#' from [OMB Circular A-11, Appendix C](https://obamawhitehouse.archives.gov/sites/default/files/omb/assets/a11_current_year/app_c.pdf)
#'
#' @param x `<chr>`  The bureau code to search for, e.g., `"38"`; if
#'   `NULL` (the default), returns all bureau codes
#'
#' @returns `<tibble>` of search results
#'
#' @examples
#' bureau_code("38")
#'
#' head(bureau_code())
#'
#' @autoglobal
#'
#' @export
bureau_code <- \(x = NULL) {

  search_in(
    get_pin("bureauCodes"),
    "bureauCode",
    x)
}
