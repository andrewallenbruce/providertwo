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


# base_url
# care_current: base_url + stats + ? + offset + limit
# care_temporal: base_url + data + offset
# default: base_url + offset + size + count + results
#' @autoglobal
#' @noRd
append_url <- function(url, api = "default") {
  paste0(url, switch(
    api,
    stats = "/stats?offset=0&size=1",
    care = "?offset=0&size=1",
    default = "?count=true&results=true&offset=0&limit=1",
  ))
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

  f <- \(x, qry = NULL) fparse(resp_body_string(x), query = qry)

  if (!is.null(query)) {
    switch(
      query,
      results    = return(f(resp) |> _[["results"]]),
      count      = return(f(resp) |> _[["count"]]),
      found_rows = return(f(resp) |> _[["found_rows"]])
      )
  }

  f(resp, qry = query)
}

#' Generate API Request "Offset" Sequence
#'
#' @param n     `<int>` Number of results in an API request
#' @param limit `<int>` API rate limit
#' @param type  `<chr>` Return type, either `"seq"` or `"size"` (default)
#' @returns Depending on `type` input and conditions listed below, returns:
#'    * `n == 0`: returns `0`
#'    * `n <= limit`: returns `n`
#'    * `n > limit`:
#'       * `type = "seq"`: integer sequence beginning at `0`, incremented by `limit`, of length equal to `ceiling(n / limit)`.
#'       * `type = "size"`: single integer equal to length of sequence returned by `type = "seq"`.
#'
#' @examplesIf interactive()
#' offset(100, 10)
#' offset(100, 10, "seq")
#'
#' offset(10, 100)
#' offset(10, 100, "seq")
#'
#' offset(47984, 5000)
#' offset(47984, 5000, "seq")
#'
#' offset(147984, 2000)
#' offset(147984, 2000, "seq")
#' @autoglobal
#' @noRd
offset <- function(n, limit, type = "size") {
  check_number_whole(n, min = 0)
  check_number_whole(limit, min = 1)

  if (n == 0L)    return(0L)
  # if (n <= limit) return(n)

  switch (
    match.arg(type, c("size", "seq")),
    size = seq_size(from = 0L, to = n, by = limit),
    seq  = seq_(from = 0L, to = n, by = limit)
  )
}

#' @autoglobal
#' @noRd
bound <- function(lower, upper) {
  check_number_whole(lower, min = 0)
  check_number_whole(upper, min = 1)
  iif(lower > upper, upper, lower, nThread = 4L, tprom = TRUE)
}
