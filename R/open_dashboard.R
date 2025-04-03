#' Open Dashboard Response Class
#'
#' @param response `<openRespDash>` object returned from the API call to the dashboard.
#'
#' @returns An S7 `<openRespDash>` object.
#'
#' @examples
#' open_dashboard()
#' @autoglobal
#' @name OpenResp
#' @export

#' @autoglobal
#' @rdname OpenResp
#' @export
openRespDash <- new_class(
  name = "openRespDash",
  package = NULL,
  properties = list(response = class_list)
)

#' @autoglobal
#' @rdname OpenResp
#' @export
open_dashboard <- function() {
  openRespDash(
    response = openCurr("dashboard") |>
      prop("identifier") |>
      request() |>
      req_url_query(
        schema  = "false",
        keys    = "true",
        results = "true",
        count   = "true",
        offset  = 0L,
        limit   = 500L
      ) |>
      req_perform() |>
      resp_body_string() |>
      fparse() |>
      _[["results"]]
  )
}

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
