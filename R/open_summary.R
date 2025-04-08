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
  prop(x, "response") |>
    as_tbl()
}

#' Open Payments Summary Dashboard
#'
#' @returns A `<tibble>`
#'
#' @examples
#' open_dashboard()
#'
#' @autoglobal
#' @rdname open_summary
#' @export
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

#' Open Payments National Overall Total and Averages
#'
#' @returns A `<tibble>`
#'
#' @examples
#' open_national()
#'
#' @autoglobal
#' @rdname open_summary
#' @export
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
