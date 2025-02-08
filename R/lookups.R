#' Public Catalog Dataset/Distribution Names
#' @autoglobal
#' @noRd
fname_to_dataset <- function(x) {
  nswitch(
    x,
    "enrollees",             "Public Provider Enrollment",  # "Medicare Fee-For-Service  Public Provider Enrollment",
    "opt_out",               "Opt Out Affidavits",
    "order_refer",           "Order and Referring",
    "reassignments",         "Revalidation Reassignment List",
    "hospitals",             "Hospital Enrollments",
    "laboratories",          "Provider of Services File - Clinical Laboratories",
    "quality_payment",       "Quality Payment Program Experience",
    "crosswalk",             "Medicare Provider and Supplier Taxonomy Crosswalk",
    "rbcs",                  "Restructured BETOS Classification System",
    "pending_nonphysicians", "Pending Initial Logging and Tracking Non Physicians",
    "pending_physicians",    "Pending Initial Logging and Tracking Physicians",
    "prescribers_drug",      "Medicare Part D Prescribers - by Provider and Drug",
    "prescribers_provider",  "Medicare Part D Prescribers - by Provider",
    "prescribers_geography", "Medicare Part D Prescribers - by Geography and Drug",
    "utilization_service",   "Medicare Physician & Other Practitioners - by Provider and Service",
    "utilization_provider",  "Medicare Physician & Other Practitioners - by Provider$|Medicare Physician & Other Practitioners - by Provider : ", # "8889d81e-2ee7-448f-8713-f071038289b5",
    "utilization_geography", "Medicare Physician & Other Practitioners - by Geography and Service",
    default = NA_character_,
    nThread = 4L
  )
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
#' @keywords internal
#'
#' @export
recode_iso8601 <- \(x) {
  nswitch(
    x,
    "R/P10Y",   "Decennially [R/P10Y]",
    "R/P4Y",    "Quadrennially [R/P4Y]",
    "R/P3Y",    "Triennially [R/P3Y]",
    "R/P2Y",    "Biennially [R/P2Y]",
    "R/P1Y",    "Annually [R/P1Y]",
    "R/P6M",    "Biannually [R/P6M]",
    "R/P4M",    "Triannually [R/P4M]",
    "R/P3M",    "Quarterly [R/P3M]",
    "R/P2M",    "Bimonthly [R/P2M]",
    "R/P1M",    "Monthly [R/P1M]",
    "R/P0.5M",  "Biweekly [R/P0.5M]",
    "R/P2W",    "Biweekly [R/P2W]",
    "R/P0.33M", "Three Times a Month [R/P0.33M]",
    "R/P1W",    "Weekly [R/P1W]",
    "R/P0.5W",  "Twice a Week [R/P0.5W]",
    "R/P3.5D",  "Twice a Week [R/P3.5D]",
    "R/P0.33W", "Three Times a Week [R/P0.33W]",
    "R/P1D",    "Daily [R/P1D]",
    "R/PT1H",   "Hourly [R/PT1H]",
    "R/PT1S",   "Continuously [R/PT1S]",
    default = NA_character_,
    nThread = 4L
  )
}

#' Roxygenise ISO 8601 Recurring Time Intervals
#' @autoglobal
#' @noRd
roxy8601 <- \(x) {
  nswitch(
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
#' @keywords internal
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
#' @keywords internal
#'
#' @export
bureau_code <- \(x = NULL) {
  search_in(
    get_pin("bureauCodes"),
    "bureauCode",
    x)
  }
