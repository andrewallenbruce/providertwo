#' Create Offset Sequence
#'
#' @param rows `<int>` The number of rows found in the dataset
#'
#' @param size `<int>` The size or limit of the offset sequence
#'
#' @returns `<int>` If `rows > size`, vector of length equal to
#'                  `rows / size + 1` (sequence begins at zero).
#'                  If `rows <= size`, the function simply returns
#'                  `rows`.
#'
#' @examples
#' offset_sequence(rows = 100, size = 10)
#'
#' offset_sequence(rows = 10, size = 100)
#'
#' offset_sequence(rows = 47984, size = 5000)
#'
#' @autoglobal
#'
#' @export
offset_sequence <- \(rows, size) {

  if (!rlang::is_integerish(rows)) {
    rlang::abort("`rows` must be integerish.", call = call("offset_sequence"))
  }
  if (!rlang::is_integerish(size)) {
    rlang::abort("`size` must be integerish.", call = call("offset_sequence"))
  }

  if (rows <= size)
    return(rows)

  0:round(rows / size) * size

}

#' ISO 8601 Recurring Time Intervals
#'
#' @source [DCAT Schema: accrualPeriodicity](https://resources.data.gov/resources/dcat-us/#accrualPeriodicity)
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
#'
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
    "R/P10Y",   "Decennially (R/P10Y)",
    "R/P4Y",    "Quadrennially (R/P4Y)",
    "R/P3Y",    "Triennially (R/P3Y)",
    "R/P2Y",    "Biennially (R/P2Y)",
    "R/P1Y",    "Annually (R/P1Y)",
    "R/P6M",    "Twice a Year (R/P6M)",
    "R/P4M",    "Three Times a Year (R/P4M)",
    "R/P3M",    "Quarterly (R/P3M)",
    "R/P2M",    "Bimonthly (R/P2M)",
    "R/P1M",    "Monthly (R/P1M)",
    "R/P0.5M",  "Biweekly (R/P0.5M)",
    "R/P2W",    "Biweekly (R/P2W)",
    "R/P0.33M", "Three Times a Month (R/P0.33M)",
    "R/P1W",    "Weekly (R/P1W)",
    "R/P0.5W",  "Twice a Week (R/P0.5W)",
    "R/P3.5D",  "Twice a Week (R/P3.5D)",
    "R/P0.33W", "Three Times a Week (R/P0.33W)",
    "R/P1D",    "Daily (R/P1D)",
    "R/PT1H",   "Hourly (R/PT1H)",
    "R/PT1S",   "Continuously Updated (R/PT1S)",
    default = NA_character_,
    nThread = 4L
  )
}

#' DCAT Schema: Program Code
#'
#' Primary program related to a data asset,
#' from the [Federal Program Inventory](https://resources.data.gov/schemas/dcat-us/v1.1/FederalProgramInventory_FY13_MachineReadable_091613.csv).
#'
#' @source [DCAT Schema: Program Code](https://resources.data.gov/resources/dcat-us/#programCode)
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
program_code <- \(x = NULL) { search_in(get_pin("programCodes"), "programCodePODfmt", x) }

#' DCAT Schema: Bureau Code
#'
#' Combined Agency and Bureau Code,
#' from [OMB Circular A-11, Appendix C](https://obamawhitehouse.archives.gov/sites/default/files/omb/assets/a11_current_year/app_c.pdf)
#'
#' @source [DCAT Schema: Bureau Code](https://resources.data.gov/resources/dcat-us/#bureauCode)
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
bureau_code <- \(x = NULL) { search_in(get_pin("bureauCodes"), "bureauCode", x) }

#' Wrapper for `terse::terse()`
#'
#' @param x `<list>` or `<data.frame>` to be printed
#' @param p `<chr>` prefix to be used for each line
#' @param w `<int>` target width; 0 = auto; -1 = no limit
#' @param m `<int>` maximum vector length anywhere in original object
#' @param s `<chr>` separator to be used for each line
#' @autoglobal
#' @keywords internal
#' @export
glimst <- \(x,
           p = "- ",
           w = 0,
           m = 20,
           s = " ") {

  terse::terse(
    x           = x,
    prefix      = p,
    width       = w,
    max_vec_len = m,
    config      = list(
      gsep      = s,
      ansi      = FALSE)
  )
}
