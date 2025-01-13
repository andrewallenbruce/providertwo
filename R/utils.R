#' Generate API Request "Offset" Sequence
#'
#' @param nobs `<int>` Number of results returned in the API request
#'
#' @param limit `<int>` Maximum number of results an API will return per request
#'
#' @returns `<int>` If `nobs > limit`, an integer sequence beginning at `0` of
#'    length equal to `nobs / limit + 1`. If `nobs <= limit`, the function
#'    simply returns `nobs`.
#'
#' @examples
#' offset_sequence(100, 10)
#'
#' offset_sequence(10, 100)
#'
#' offset_sequence(47984, 5000)
#'
#' offset_sequence(147984, 2000)
#'
#' @autoglobal
#'
#' @importFrom cheapr seq_
#'
#' @export
offset_sequence <- \(nobs, limit) {

  if (!is_integerish(c(nobs, limit), n = 2)) {
    abort(
      "Both `nobs` and `limit` must be whole numbers.",
      call = call("offset_sequence")
      )
    }

  if (nobs <= limit) return(nobs)

  c(0, seq_(limit, nobs, by = limit) + 1, nobs)
}

#' Recode ISO 8601 Recurring Time Intervals
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
    "R/P10Y",   "[R/P10Y] Decennially",
    "R/P4Y",    "[R/P4Y] Quadrennially",
    "R/P3Y",    "[R/P3Y] Triennially",
    "R/P2Y",    "[R/P2Y] Biennially",
    "R/P1Y",    "[R/P1Y] Annually",
    "R/P6M",    "[R/P6M] Biannually",
    "R/P4M",    "[R/P4M] Triannually",
    "R/P3M",    "[R/P3M] Quarterly",
    "R/P2M",    "[R/P2M] Bimonthly",
    "R/P1M",    "[R/P1M] Monthly",
    "R/P0.5M",  "[R/P0.5M] Biweekly",
    "R/P2W",    "[R/P2W] Biweekly",
    "R/P0.33M", "[R/P0.33M] Three Times a Month",
    "R/P1W",    "[R/P1W] Weekly",
    "R/P0.5W",  "[R/P0.5W] Twice a Week",
    "R/P3.5D",  "[R/P3.5D] Twice a Week",
    "R/P0.33W", "[R/P0.33W] Three Times a Week",
    "R/P1D",    "[R/P1D] Daily",
    "R/PT1H",   "[R/PT1H] Hourly",
    "R/PT1S",   "[R/PT1S] Continuously",
    default = NA_character_,
    nThread = 4L
  )
}

#' Roxygenise ISO 8601 Recurring Time Intervals
#' @autoglobal
#' @noRd
roxy8601 <- \(x) {
  kit::nswitch(
    x,
    "R/P10Y",   "Decennially (R/P10Y)",
    "R/P4Y",    "Quadrennially (R/P4Y)",
    "R/P3Y",    "Triennially (R/P3Y)",
    "R/P2Y",    "Biennially (R/P2Y)",
    "R/P1Y",    "Annually (R/P1Y)",
    "R/P6M",    "Biannually (R/P6M)",
    "R/P4M",    "Triannually (R/P4M)",
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
    "R/PT1S",   "Continuously (R/PT1S)",
    default = NA_character_,
    nThread = 4L
  )
}

#' Program Code
#'
#' Code for primary program related to a data asset, from the [Federal Program Inventory](https://resources.data.gov/schemas/dcat-us/v1.1/FederalProgramInventory_FY13_MachineReadable_091613.csv).
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

#' Bureau Code
#'
#' Combined Agency and Bureau Code, from the [OMB Circular A-11, Appendix C (PDF)](https://obamawhitehouse.archives.gov/sites/default/files/omb/assets/a11_current_year/app_c.pdf)
#'
#' @source [DCAT Schema: Bureau Code](https://resources.data.gov/resources/dcat-us/#bureauCode)
#'
#' @param x `<chr>` The bureau code to search for, e.g., `"38"`; if
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

#' @noRd
debugme_on <- \() Sys.setenv(DEBUGME = "providertwo")

#' @noRd
debugme_off <- \() Sys.unsetenv("DEBUGME")

#' @noRd
is_debuggingme <- \() identical(Sys.getenv("DEBUGME"), "providertwo")

#' @noRd
online <- \() curl::has_internet()
