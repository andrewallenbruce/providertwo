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

#' Parse ISO8601 Recurrence Rules
#'
#' @param x `<chr>` vector of ISO8601 recurrence rules
#'
#' @returns `<chr>` vector of human-readable recurrence rule descriptions
#'
#' @examples
#' data.frame(
#'   accrualPeriodicity = c(
#'   "R/PT1S",   "R/PT1H",  "R/P1D", "R/P3.5D",
#'   "R/P0.33W", "R/P0.5W", "R/P1W", "R/P2W",
#'   "R/P0.33M", "R/P0.5M", "R/P1M", "R/P2M",
#'   "R/P3M",    "R/P4M",   "R/P6M", "R/P1Y",
#'   "R/P2Y",    "R/P3Y",   "R/P4Y", "R/P10Y"),
#'   accrualDescription = recode_iso8601(accrualPeriodicity)
#' )
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
