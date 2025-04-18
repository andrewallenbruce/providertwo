#' @include S7_care.R
#' @include S7_pro.R
#' @include S7_open.R
NULL

#' @name new_request
#'
#' @title Create a new request by class
#'
#' @param x An object of class `proMain`, `careMain`, or `openMain`
#'
#' @returns A new request
#'
#' @examples
#' careMain("enrollees") |> new_request()
#' proMain("affiliations") |> new_request()
#' openMain("dashboard") |> new_request()
#'
#' @autoglobal
#' @export
new_request <- new_generic("new_request", "x", function(x) {
  S7_dispatch()
})

method(new_request, careMain) <- function(x) {
  prop(x, "identifier") |>
    request() |>
    req_url_query(offset = 0L, size = 5000L)
}

method(new_request, proMain) <- function(x) {
  prop(x, "identifier") |>
    request() |>
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
  prop(x, "identifier") |>
    request() |>
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
