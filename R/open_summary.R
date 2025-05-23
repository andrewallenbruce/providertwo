#' @autoglobal
#' @noRd
openDashboard <- new_class(
  name       = "openDashboard",
  package    = NULL,
  properties = list(response = class_list)
)

#' @autoglobal
#' @noRd
openNational <- new_class(
  name       = "openNational",
  package    = NULL,
  properties = list(response = class_list)
)

#' @autoglobal
#' @noRd
tidyup <- new_generic("tidyup", "x", function(x) {
  S7_dispatch()
})

method(tidyup, openDashboard) <- function(x) {

  cnames <- c(
    `2017` = "py_2017",
    `2018` = "py_2018",
    `2019` = "py_2019",
    `2020` = "py_2020",
    `2021` = "py_2021",
    `2022` = "py_2022",
    `2023` = "py_2023",
    "ALL"  = "total")

  x <- mtt(
      prop(x, "response"),
      metric               = data_metrics,
      data_metrics         = NULL,
      dashboard_row_number = NULL,
      type                 = case(
        greplace(metric, "^Total Number of") ~ "count",
        greplace(metric, "^Total Dollar Amount of") ~ "dollar",
        greplace(metric, "^Total Dollar Amount Invested") ~ "invest",
        greplace(metric, "^Total value of interest") ~ "value")) |>
    rsplit(~ type)

  x$count <-  mtt(
    x$count,
    metric = greplace(metric, "^Total Number of ", ""),
    metric = greplace(metric, "^all ", "", metric),
    type   = ifelse(greplace(metric, "^Payment Records"), "record", "entity")) |>
    rsplit( ~ type)

  x$count$entity <- x$count$entity |>
    rnm(cnames) |>
    mtt(
      metric = greplace(metric, "Companies Making Payments \\(AM/GPO Making Payment\\)", "Companies [AM/GPO] Making Payments"),
      metric = greplace(metric, "Physician Covered Recipients with associated payment records", "Physicians with Payment Records"),
      metric = greplace(metric, "Non-Physician Practitioner Covered Recipients with associated payment records", "NPPs with Payment Records"),
      metric = greplace(metric, "Teaching Hospitals with associated payment records", "Teaching Hospitals with Payment Records")) |>
    pivot(ids   = "metric",
          names = list("year", "count")) |>
    roworder(metric, -year) |>
    colorder(year) |>
    mtt(count = as.integer(count)) |>
    as_tbl()

  x$count$record <- x$count$record |>
    rnm(cnames) |>
    mtt(type = case(
      gdetect(metric, "General Payments") ~ "General",
      gdetect(metric, "Research Payments") ~ "Research",
      gdetect(metric, "Ownership Payments") ~ "Ownership",
      .default = "All"),
      status = case(
        gdetect(metric, "Disputed") ~ "Disputed",
        gdetect(metric, "Undisputed") ~ "Undisputed"),
      entity = case(
        gdetect(metric, "- Covered Recipient") ~ "Covered Recipient",
        gdetect(metric, "- Non-Covered Recipient") ~ "Non-Covered Recipient",
        gdetect(metric, "- Physician or Non-Covered Recipient") ~ "Physician or Non-Covered Recipient",
        gdetect(metric, "- Non-Physician Practitioner or Non-Covered Recipient") ~ "Non-Physician Practitioner or Non-Covered Recipient",
        gdetect(metric, " to Physicians") ~ "Physician",
        gdetect(metric, " to Non-Physician Practitioners") ~ "Non-Physician Practitioner",
        gdetect(metric, " to Teaching Hospitals") ~ "Teaching Hospital",
        gdetect(metric, " - Non-Covered Recipient Entity") ~ "Non-Covered Recipient Entity",
        gdetect(metric, " - Non-Covered Recipient Individual") ~ "Non-Covered Recipient Individual"),
      metric = NULL) |>
    pivot(ids   = c("type", "status", "entity"),
          names = list("year", "count")) |>
    roworder(type, -year) |>
    colorder(year, type, entity) |>
    mtt(count = as.integer(count)) |>
    as_tbl()

  x$invest <- mtt(
    x$invest,
    metric = greplace(metric, "^Total Dollar Amount Invested of all Payment Records ", ""),
    metric = greplace(metric, "^-\\s", ""))

  x$value <- mtt(
    x$value,
    metric = greplace(metric, "^Total value of interest of all Payment Records ", ""),
    metric = greplace(metric, "^-\\s", ""))

  x$dollar <- mtt(
    x$dollar,
    metric = greplace(metric, "^Total Dollar Amount of ", ""),
    metric = greplace(metric, "^all ", ""))

  x
}

method(tidyup, openNational) <- function(x) {

  phy <- c(
    year            = "program_year",
    payment_type    = "payment_type",
    recipient_type  = "recipient_type",
    recipient_n     = "total_number_of_physicians",
    payment_sum     = "total_payment_amount_physician",
    payment_mean    = "mean_total_payment_amount_physician",
    payment_median  = "median_total_payment_amount_physician",
    payments_n      = "total_payment_count_physician",
    payments_mean   = "mean_total_payment_count_physician",
    payments_median = "median_total_payment_count_physician"
  )

  npp <- c(
    year            = "program_year",
    payment_type    = "payment_type",
    recipient_type  = "recipient_type",
    recipient_n     = "total_number_of_non_physician_practitioners",
    payment_sum     = "total_payment_amount_non_physician_practitioner",
    payment_mean    = "mean_total_payment_amount_non_physician_practitioner",
    payment_median  = "median_total_payment_amount_non_physician_practitioner",
    payments_n      = "total_payment_count_non_physician_practitioner",
    payments_mean   = "mean_total_payment_count_non_physician_practitioner",
    payments_median = "median_total_payment_count_non_physician_practitioner"
  )

  x <- prop(x, "response") |>
    as_tbl() |>
    mtt(recipient_type = clean_names(recipient_type)) |>
    rsplit( ~ recipient_type)

  x$covered_recipient_physician <- mtt(
    x$covered_recipient_physician,
    program_year = factor_(program_year),
    payment_type = factor_(payment_type),
    recipient_type = "Physician",
    across(c(total_number_of_physicians,
             total_payment_count_physician), as.integer),
    across(c(total_payment_amount_physician,
             mean_total_payment_amount_physician,
             median_total_payment_amount_physician), as.numeric),
    across(c(mean_total_payment_count_physician,
             median_total_payment_count_physician), as.double)) |>
    rnm(phy)

  x$covered_recipient_non_physician_practitioner <- mtt(
    x$covered_recipient_non_physician_practitioner,
    program_year = factor_(program_year),
    payment_type = factor_(payment_type),
    recipient_type = "Non-Physician Practitioner",
    across(c(total_number_of_non_physician_practitioners,
             total_payment_count_non_physician_practitioner), as.integer),
    across(c(total_payment_amount_non_physician_practitioner,
             mean_total_payment_amount_non_physician_practitioner,
             median_total_payment_amount_non_physician_practitioner), as.numeric),
    across(c(mean_total_payment_count_non_physician_practitioner,
             median_total_payment_count_non_physician_practitioner), as.double)) |>
    rnm(npp)

  x |>
    rowbind() |>
    mtt(recipient_type = factor_(recipient_type)) |>
    roworder(recipient_type, payment_type)
}

#' Open Payments Summaries
#' @name open_summary
#' @returns A `<tibble>`
#' @examplesIf interactive()
#' open_dashboard()
#' open_national()
NULL

#' @autoglobal
#' @export
#' @rdname open_summary
open_dashboard <- function() {
  openDashboard(
    response = open_endpoint("summary_dashboard") |>
      quick_(offset = 0L, limit = 500L)) |>
    tidyup()
}

#' @autoglobal
#' @export
#' @rdname open_summary
open_national <- function() {
  openNational(
    response = open_endpoint("summary_national") |>
      quick_(offset = 0L, limit = 500L)) |>
    tidyup()
}
