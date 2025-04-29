#' @include S7_care.R
#' @include S7_pro.R
#' @include S7_open.R
NULL

#' @name new_request
#' @title Create a new request by class
#' @param x An object of class `pro_endpoint`, `careMain`, or `open_endpoint`
#' @returns A new request
#' @examples
#' careMain("enrollees") |> new_request()
#' pro_endpoint("PDC_affiliations") |> new_request()
#' open_endpoint("PROF_covered") |> new_request()
#' careGroup("hospital") |> new_request()
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

method(new_request, careMain) <- function(x) {
  identifier(x) |>
    new_request() |>
    req_url_query(offset = 0L, size = 5000L)
}

method(new_request, careGroup) <- function(x) {
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
