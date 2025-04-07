#' @autoglobal
#' @noRd
fmt_contactpoint <- function(x) {
  x <- delist(get_elem(x, "^has", regex = TRUE)) |>
    set_names(delist(get_elem(x, "fn")))

  as.character(glue("{names(x)} ({x})"))
}

#' @autoglobal
#' @noRd
fmt_temporal <- function(x) {
  gsub("/", paste0(" ", cli::symbol$bullet, " "), x, perl = TRUE)
}

#' @autoglobal
#' @noRd
join_on_title <- function(a, b) {
  join(
    x = a,
    y = b,
    on = "title",
    verbose = 0
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
#' @examplesIf rlang::is_interactive()
#' accrualPeriodicity = c(
#'   "R/PT1S",   "R/PT1H",  "R/P1D", "R/P3.5D",
#'   "R/P0.33W", "R/P0.5W", "R/P1W", "R/P2W",
#'   "R/P0.33M", "R/P0.5M", "R/P1M", "R/P2M",
#'   "R/P3M",    "R/P4M",   "R/P6M", "R/P1Y",
#'   "R/P2Y",    "R/P3Y",   "R/P4Y", "R/P10Y")
#'
#' fmt_periodicity(accrualPeriodicity)
#'
#' @section References:
#'
#' - [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601)
#' - [ISO 8601 Repeating_intervals](https://en.wikipedia.org/wiki/ISO_8601#Repeating_intervals)
#' - [Recurring Time Intervals](https://sentenz.github.io/convention/convention/iso-8601/#19-recurring-time-intervals)
#'
#' @autoglobal
#' @noRd
fmt_periodicity <- function(x) {
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
#'
#' @param x `<chr>` vector of ISO8601 recurrence rules
#'
#' @autoglobal
#' @noRd
roxy8601 <- function(x) {
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

#' @noRd
pro_url <- function(x) paste0("https://data.cms.gov/provider-data/api/1/datastore/query/", x, "/0")

#' @noRd
pro_dict <- function(x) paste0("https://data.cms.gov/provider-data/dataset/", x, "#data-dictionary")

#' @noRd
open_url <- function(x) paste0("https://openpaymentsdata.cms.gov/api/1/datastore/query/", x, "/0")

#' @noRd
caid_url <- function(x) paste0("https://data.medicaid.gov/api/1/datastore/query/", x, "/0")

#' Parse datetime
#'
#' @param x `<chr>` vector to parse; format: "YYYY-MM-DDTHH:MM:SS"
#'
#' @returns `<chr>` parsed ISOdatetime vector
#'
#' @examplesIf rlang::is_interactive()
#' as_datetime("2024-07-29T20:37:53")
#'
#' @seealso [clock::date_time_parse_RFC_3339()]
#' @autoglobal
#' @noRd
as_datetime <- function(x) {
  ISOdatetime(
    substr(x, 1, 4),
    substr(x, 6, 7),
    substr(x, 9, 10),
    substr(x, 12, 13),
    substr(x, 15, 16),
    substr(x, 18, 19)
  )
}

#' Parse `openFDA` date character vectors
#' @autoglobal
#' @noRd
as_fda_date <- function(i) {
  delist(map(i, function(x)
    paste0(
      substr(x, 1, 4),
      substr(x, 5, 6),
      substr(x, 7, 8),
      collapse = "-"
    )))
}
