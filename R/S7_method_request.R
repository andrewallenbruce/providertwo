#' @include S7_backend.R
NULL

#' @name new_request
#'
#' @title Create a new request by class
#'
#' @param x An object of class `proMain`, `MainCurrent`, or `openMain`
#'
#' @param ... Additional arguments?
#'
#' @returns A new request
#'
#' @examples
#' MainCurrent("enrollees") |> new_request()
#' proMain("affiliations") |> new_request()
#' openMain("dashboard") |> new_request()
#'
#' @autoglobal
#' @export
new_request <- new_generic("new_request", "x")

method(new_request, MainCurrent) <- function(x) {
  request(x@identifier) |>
    req_url_query(offset = 0L, size = 5000L)
}

method(new_request, proMain) <- function(x) {
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

method(new_request, openMain) <- function(x) {
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
