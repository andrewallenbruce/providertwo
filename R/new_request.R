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

#' @name list_resources
#'
#' @title List resources from a `MainCurrent` or `class_character` object
#'
#' @param x An object of class `MainCurrent` or `class_character`
#'
#' @param ... Additional arguments?
#'
#' @returns A list of API resources
#'
#' @examples
#' MainCurrent("enrollees") |> list_resources()
#'
#' @autoglobal
#' @export
list_resources <- new_generic("list_resources", "x")

method(list_resources, MainCurrent) <- function(x) {
  fload(x@resources, query = "/data") |>
    fcompute(
      file = name,
      size = roundup(fileSize / 1e6),
      ext = file_ext(downloadURL),
      download = downloadURL
    ) |>
    roworder(ext, -size)
}

method(list_resources, class_character) <- function(x) {
  fload(x, query = "/data") |>
    fcompute(
      file = name,
      size = roundup(fileSize / 1e6),
      ext = file_ext(downloadURL),
      download = downloadURL
    ) |>
    roworder(ext, -size)
}
