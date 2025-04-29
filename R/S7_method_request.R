#' @include S7_care.R
#' @include S7_pro.R
#' @include S7_open.R
#' @include S7_caid.R
NULL

#' @name new_request
#' @title Create a new request by class
#' @param x An object of class `care_endpoint`, `care_group`, `pro_endpoint`, or `open_endpoint`
#' @returns A new request
#' @examples
#' care_endpoint("enrollees") |> new_request()
#' care_group("hospital") |> new_request()
#'
#' pro_endpoint("PDC_affiliations") |> new_request()
#'
#' open_endpoint("PROF_covered") |> new_request()
#'
#' @autoglobal
#' @export
new_request <- new_generic("new_request", "x", function(x) {
  S7_dispatch()
})

method(new_request, class_character) <- function(x) {
  x |>
    request() |>
    req_throttle(capacity = 30, fill_time_s = 60)
}

method(new_request, care_endpoint) <- function(x) {
  identifier(x) |>
    new_request() |>
    req_url_query(offset = 0L, size = 5000L)
}

method(new_request, care_group) <- function(x) {
  map(
    members(x),
    \(x) identifier(x) |>
      new_request() |>
      req_url_query(offset = 0L, size = 5000L)
  )
}

method(new_request, pro_endpoint) <- function(x) {
  identifier(x) |>
    new_request() |>
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

method(new_request, open_endpoint) <- function(x) {
  identifier(x) |>
    new_request() |>
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
