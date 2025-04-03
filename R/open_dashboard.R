#' @autoglobal
#' @noRd
openDashboard <- new_class(
  name       = "openDashboard",
  package    = NULL,
  properties = list(response = class_list)
)

#' Open Payments Summary Dashboard
#' @returns A `<tibble>`
#' @examples
#' open_dashboard()
#' @autoglobal
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

# x <- open_dashboard()
# x |>
#   fastplyr::f_arrange(data_metrics, dashboard_row_number) |>
#   fcompute(
#     id = collapse::groupid(dashboard_row_number),
#     grp = collapse::groupid(data_metrics),
#     metric = data_metrics
#   ) |>
#   print(n = Inf)

#   x |>
#     # fastplyr::f_arrange(data_metrics, dashboard_row_number)
#     fcompute(
#       id = collapse::groupid(dashboard_row_number),
#       metric = data_metrics) |>
#     print(n = Inf)
# }
#
# dashboard |>
#   as_tbl() |>
#   f_select(Data_Metrics, Total) |>
#   f_arrange(-Total) |>
#   print(n = Inf)
#
# dashboard |>
#   as_tbl() |>
#   f_select(Data_Metrics, Total) |>
#   f_arrange(-Total) |>
#   print(n = Inf)
#
# dash <- dashboard |>
#   as_tbl() |>
#   slt(-Dashboard_Row_Number) |>
#   pivot(ids = 1) |>
#   roworder(Data_Metrics) |>
#   mtt(year = delist(stri_extract_all_regex(variable, "[0-9]{4}")))
#
# dash |>
#   sbt(na(year), metrics = Data_Metrics, amount = value)
#
# dash |>
#   sbt(not_na(year), year, metrics = Data_Metrics, amount = value) |>
#   mtt(year = as_int(year)) |>
#   rsplit(~ metrics)
