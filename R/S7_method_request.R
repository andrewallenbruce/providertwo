#' @include S7_care.R
#' @include S7_pro.R
#' @include S7_open.R
#' @include S7_caid.R
NULL

#' @name base_request
#' @title Create a new request by class
#' @param x An object of class `care_endpoint`, `care_group`, `pro_endpoint`, or `open_endpoint`
#' @returns A new base request
#' @examples
#' care_endpoint("enrollees") |> base_request()
#' care_group("hospital") |> base_request()
#' pro_endpoint("PDC_affiliations") |> base_request()
#' open_endpoint("PROF_covered") |> base_request()
#' @autoglobal
#' @export
base_request <- new_generic("base_request", "x", function(x) {
  S7_dispatch()
})

# method(new_request, class_endpoint) <- function(x) {
#   x |>
#     request() |>
#     req_throttle(capacity = 30, fill_time_s = 60)
# }

method(base_request, care_endpoint) <- function(x) {
  x@identifier |>
    request() |>
    req_throttle(capacity = 30, fill_time_s = 60) |>
    req_url_query(offset = 0L, size = 5000L)
}

method(base_request, care_group) <- function(x) {
  x@members |>
    map(\(x) x@identifier |>
          request() |>
          req_throttle(capacity = 30, fill_time_s = 60) |>
          req_url_query(offset = 0L, size = 5000L)
  )
}

method(base_request, pro_endpoint) <- function(x) {
  x@identifier |>
    request() |>
    req_throttle(capacity = 30, fill_time_s = 60) |>
    req_url_query(
      count   = "false",
      format  = "json",
      keys    = "true",
      limit   = 2000L,
      offset  = 0L,
      results = "true",
      rowIds  = "false",
      schema  = "false"
    )
}

method(base_request, open_endpoint) <- function(x) {
  x@identifier |>
    request() |>
    req_throttle(capacity = 30, fill_time_s = 60) |>
    req_url_query(
      count   = "false",
      format  = "json",
      keys    = "true",
      limit   = 500L,
      offset  = 0L,
      results = "true",
      rowIds  = "false",
      schema  = "false"
    )
}
