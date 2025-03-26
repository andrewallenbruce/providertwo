#' @include S7_backend.R
NULL

#' @name new_request
#'
#' @title Create a new request by class
#'
#' @param x An object of class `ProviderCurrent`, `MainCurrent`, or `OpenCurrent`
#'
#' @param ... Additional arguments?
#'
#' @returns A new request
#'
#' @examples
#' MainCurrent("enrollees") |> new_request()
#' ProviderCurrent("affiliations") |> new_request()
#'
#' @autoglobal
#' @export
new_request <- new_generic("new_request", "x")

method(new_request, MainCurrent) <- function(x) {
  request(x@identifier) |>
    req_url_query(offset = 0L, size = 5000L)
}

method(new_request, ProviderCurrent) <- function(x) {
  request(x@identifier) |>
    req_url_query(
      schema = "false",
      keys   = "true",
      rowIds = "false",
      offset = 0L,
      limit  = 2000L
    )
}

method(new_request, OpenCurrent) <- function(x) {
  request(x@identifier) |>
    req_url_query(
      schema = "false",
      keys   = "true",
      offset = 0L,
      limit  = 500L
    )
}
