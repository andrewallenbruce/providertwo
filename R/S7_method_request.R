#' @include S7_backend.R
NULL

#' @name new_request
#'
#' @title Create a new request by class
#'
#' @param x An object of class `proCurr`, `MainCurrent`, or `openCurr`
#'
#' @param ... Additional arguments?
#'
#' @returns A new request
#'
#' @examples
#' MainCurrent("enrollees") |> new_request()
#' proCurr("affiliations") |> new_request()
#' openCurr("dashboard") |> new_request()
#'
#' @autoglobal
#' @export
new_request <- new_generic("new_request", "x")

method(new_request, MainCurrent) <- function(x) {
  request(x@identifier) |>
    req_url_query(offset = 0L, size = 5000L)
}

method(new_request, proCurr) <- function(x) {
  request(x@identifier) |>
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

method(new_request, openCurr) <- function(x) {
  request(x@identifier) |>
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
