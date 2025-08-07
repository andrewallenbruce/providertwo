#' @autoglobal
#' @noRd
api_limit <- function(api) {
  switch(
    match.arg(api, c("care", "caid", "prov", "hgov", "open")),
    care   = 5000L,
    caid   = 8000L,
    prov   = 1500L,
    hgov   = 500L,
    open   = 500L
  )
}

#' @autoglobal
#' @noRd
append_url <- function(url, api = "default") {
  paste0(
    url,
    switch(
      match.arg(api, c("default", "care")),
      default = "/0?count=true&results=true&offset=0&limit=1",
      care = "?offset=0&size=1"))
}

#' @autoglobal
#' @noRd
path_stats <- function(req) {
  req_url_path_append(req, "stats")
}

#' @autoglobal
#' @noRd
is_complete_with_limit <- function(limit) {
  function(resp)
    length(resp_body_json(resp)$data) < limit
}

#' @autoglobal
#' @noRd
req_perform_iterative_offset <- function(req, limit) {
  # TODO allow switching between different API limits?
  check_number_whole(limit, min = 1, max = 8000)

  req_perform_iterative(
    req,
    next_req        = iterate_with_offset(
      param_name    = "offset",
      start         = 0L,
      offset        = limit,
      resp_complete = is_complete_with_limit(limit)
    )
  )
}

#' @autoglobal
#' @noRd
resp_simple_json <- function(resp, ...) {
  resp_body_json(resp, simplifyVector = TRUE, check_type = FALSE, ...)
}

#' @autoglobal
#' @noRd
perform_simple <- function(req, ...) {
  req_perform(req) |> resp_simple_json(...)
}

#' @autoglobal
#' @noRd
perform_simple_request <- function(x, ...) {
  x |>
    request() |>
    req_perform() |>
    resp_simple_json(...)
}

#' @autoglobal
#' @noRd
parse_string <- function(resp, query = NULL) {

  if (!is.null(query)) {
    switch(
      query,
      results = return(fparse(resp_body_string(resp)) |> _[["results"]]),
      count = return(fparse(resp_body_string(resp)) |> _[["count"]]),
      found_rows = return(fparse(resp_body_string(resp)) |> _[["found_rows"]])
      )
  }

  fparse(resp_body_string(resp), query = query)
}

#' Generate API Request "Offset" Sequence
#'
#' @param n `<int>` Number of results returned in an API request
#'
#' @param limit `<int>` API rate limit
#'
#' @returns `<int>` If `n <= limit`, simply returns `n`. If `n > limit`,
#'   returns an integer sequence beginning at `0`, of length equal to
#'   `ceiling(n / limit)`.
#'
#' @examplesIf interactive()
#' offset_seq(100, 10)
#' offset_size(100, 10)
#'
#' offset_seq(10, 100)
#' offset_size(10, 100)
#'
#' offset_seq(47984, 5000)
#' offset_size(47984, 5000)
#'
#' offset_seq(147984, 2000)
#' offset_size(147984, 2000)
#' @name offset
#' @noRd
NULL

#' @rdname offset
#' @autoglobal
#' @noRd
offset_seq <- function(n, limit) {

  check_number_whole(n, min = 0)
  check_number_whole(limit, min = 1)

  if (n <= limit) return(n)

  seq_(from = 0L, to = n, by = limit)
}

#' @rdname offset
#' @autoglobal
#' @noRd
offset_size <- function(n, limit) {

  check_number_whole(n, min = 0)
  check_number_whole(limit, min = 1)

  if (n <= limit) return(1L)

  seq_size(from = 0L, to = n, by = limit)
}

#' @autoglobal
#' @noRd
bound <- function(lower, upper) {
  check_number_whole(lower, min = 0)
  check_number_whole(upper, min = 1)
  iif(lower > upper, upper, lower, nThread = 4L, tprom = TRUE)
}
