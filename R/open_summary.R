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
  prop(x, "response") |>
    slt(-dashboard_row_number) |>
    as_tbl()
}

method(tidyup, openNational) <- function(x) {
  x <- prop(x, "response") |>
    map_na_if() |>
    mtt(recipient_type = clean_names(recipient_type)) |>
    rsplit( ~ recipient_type)

  x$covered_recipient_physician <- x$covered_recipient_physician |>
    fcompute(
      year = factor_(program_year),
      payment_type = factor_(payment_type),
      recipient_type = "Physician",
      recipient_n = as.integer(total_number_of_physicians),
      payment_sum = as.numeric(total_payment_amount_physician),
      payment_mean = as.numeric(mean_total_payment_amount_physician),
      payment_median = as.numeric(median_total_payment_amount_physician),
      payments_n = as.integer(total_payment_count_physician),
      payments_mean = as.double(mean_total_payment_count_physician),
      payments_median = as.double(median_total_payment_count_physician)
    )

  x$covered_recipient_non_physician_practitioner <- x$covered_recipient_non_physician_practitioner |>
    fcompute(
      year = factor_(program_year),
      payment_type = factor_(payment_type),
      recipient_type = "Non-Physician Practitioner",
      recipient_n = as.integer(total_number_of_non_physician_practitioners),
      payment_sum = as.numeric(total_payment_amount_non_physician_practitioner),
      payment_mean = as.numeric(mean_total_payment_amount_non_physician_practitioner),
      payment_median = as.numeric(median_total_payment_amount_non_physician_practitioner),
      payments_n = as.integer(total_payment_count_non_physician_practitioner),
      payments_mean = as.double(mean_total_payment_count_non_physician_practitioner),
      payments_median = as.double(median_total_payment_count_non_physician_practitioner)
    )

  x |>
    rowbind() |>
    mtt(recipient_type = factor_(recipient_type)) |>
    roworder(recipient_type, payment_type) |>
    as_tbl()
}

#' Open Payments Summaries
#'
#' @name open_summary
#'
#' @returns A `<tibble>`
#'
#' @examples
#' open_dashboard()
#' open_national()
NULL

#' @name open_dashboard
#' @title Open Payments Dashboard
#' @autoglobal
#' @export
#' @rdname open_summary
open_dashboard <- function() {
  openDashboard(
    response = openMain("dashboard") |>
      new_request() |>
      req_perform() |>
      resp_body_string() |>
      fparse() |>
      _[["results"]]
  ) |>
    tidyup()
}

#' @name open_national
#' @title Open Payments National Overall Total and Averages
#' @autoglobal
#' @export
#' @rdname open_summary
open_national <- function() {
  openNational(
    response = openMain("national_total") |>
      new_request() |>
      req_perform() |>
      resp_body_string() |>
      fparse() |>
      _[["results"]]
  ) |>
    tidyup()
}
