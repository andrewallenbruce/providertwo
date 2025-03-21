#' @include S7_backend.R
NULL

new_request <- new_generic("new_request", "x")

method(new_request, CurrentMain) <- function(x) {
  request(x@identifier) |>
    req_url_query(offset = 0L, size   = 5000L)
}

method(new_request, CurrentProvider) <- function(x) {
  request(x@identifier) |>
    req_url_query(
      schema = "false",
      keys   = "false",
      rowIds = "false",
      offset = 0L,
      limit  = 2000L
    )
}

method(new_request, CurrentOpen) <- function(x) {
  request(x@identifier) |>
    req_url_query(
      schema = "false",
      keys   = "false",
      offset = 0L,
      limit  = 500L
    )
}

list_resources <- new_generic("list_resources", "x")

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

method(list_resources, CurrentMain) <- function(x) {
  fload(x@resources, query = "/data") |>
    fcompute(
      file = name,
      size = roundup(fileSize / 1e6),
      ext = file_ext(downloadURL),
      download = downloadURL
    ) |>
    roworder(ext, -size)
}
