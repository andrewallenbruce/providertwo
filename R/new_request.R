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
