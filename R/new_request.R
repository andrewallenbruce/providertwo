new_request <- new_generic("new_request", "x")

method(new_request, CurrentMain) <- function(x) {
  request(x@identifier) |>
    req_url_query(size = 5000L)
}

method(new_request, CurrentProvider) <- function(x) {
  request(x@identifier) |>
    req_url_query(size = 2000L)
}

method(new_request, CurrentOpen) <- function(x) {
  request(x@identifier) |>
    req_url_query(size = 500L)
}
