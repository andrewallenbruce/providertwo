#' Parse Simple JSON Response
#'
#' @param resp       `<httr2_response>` object
#' @param ...        Additional arguments
#' @returns `<list>` Parsed JSON response
#'
#' @autoglobal
#' @keywords internal
#' @export
resp_simple_json <- function(resp, ...) {
  resp_body_json(resp, simplifyVector = TRUE, check_type = FALSE, ...)
}

#' Perform Request and Parse Simple JSON Response
#'
#' @param req        `<httr2_request>` object
#' @param ...        Additional arguments
#' @returns `<list>` Parsed JSON response
#'
#' @autoglobal
#' @keywords internal
#' @export
perform_simple <- function(req, ...) {
  req_perform(req, ...) |>
    resp_simple_json()
}

#' Helper for `iterate_with_offset`
#'
#' @param limit `<int>`  API rate limit
#' @returns `<function>` Function to check if API request is complete
#'
#' @autoglobal
#' @keywords internal
#' @export
is_complete_with_limit <- function(limit) {
  function(resp)
    length(resp_body_json(resp)$data) < limit
}

#' Perform Iterative Request with Offset
#'
#' @param req `<httr2_request>` object
#' @param limit `<int>` API rate limit, i.e. the maximum number of results an
#'                      API will return per request.
#' @returns list of `<httr2_response>`s
#' @keywords internal
#' @export
req_perform_iterative_offset <- function(req, limit) {
  # TODO: allow switching between different API limits?
  check_number_whole(limit, min = 1, max = 5000)

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

#' Parse JSON response
#'
#' @param resp `<httr_response>` object
#'
#' @returns `<tibble>` Parsed JSON response
#'
#' @autoglobal
#' @keywords internal
#' @export
parse_json_response_public <- function(resp) {
  resp_body_string(resp) |>
    fparse(query = "/data") |>
    as_tbl()
}

#' Parse JSON response
#'
#' @param resp `<httr_response>` object
#'
#' @returns `<tibble>`  Parsed JSON response
#'
#' @autoglobal
#' @keywords internal
#' @export
parse_json_response_provider <- function(resp) {
  resp_body_string(resp) |>
    fparse(query = "/results") |>
    as_tbl()
}

#' Mapped Parse JSON response
#'
#' @param resp `<httr_response>` object
#'
#' @returns `<tibble>` Parsed JSON response
#'
#' @autoglobal
#' @keywords internal
#' @export
map_parse_json_response_public <- function(resp) {
  map(resp, function(x)
    parse_json_response_public(x)) |>
    rowbind()
}

#' Parse JSON response
#'
#' @param resp `<httr_response>` object
#'
#' @returns `<tibble>`  Parsed JSON response
#'
#' @autoglobal
#' @keywords internal
#' @export
map_parse_json_response_provider <- function(resp) {
  map(resp, function(x)
    parse_json_response_provider(x)) |>
    rowbind()
}

#' Clean JSON response
#'
#' @param x `<data.frame>` tibble of API response
#'
#' @param names `<chr>` vector of column names
#'
#' @returns `<tibble>` Parsed JSON response as tibble
#'
#' @autoglobal
#' @keywords internal
#' @export
tidyup <- function(x, names) {
  set_names(x, names(names)) |>
    map_na_if()
}

#' Request number of results
#'
#' @param url `<chr>` API URL
#'
#' @returns `<int>` Number of results
#'
#' @autoglobal
#' @keywords internal
#' @export
nrows_public <- function(url) {

  request(url) |>
    req_url_path_append("stats") |>
    req_perform() |>
    resp_simple_json() |>
    _[["data"]] |>
    _[["found_rows"]]
}

#' Request number of results
#'
#' @param url `<chr>` API URL
#'
#' @returns `<int>` Number of results
#'
#' @autoglobal
#' @keywords internal
#' @export
nrows_provider <- function(url) {
  request(url) |>
    req_url_query(
      limit   = 1,
      offset  = 0,
      count   = "true",
      results = "false",
      schema  = "false") |>
    req_perform() |>
    resp_simple_json() |>
    _[["count"]]
}

#' Request field names
#'
#' @param url `<chr>` API URL
#'
#' @returns `<chr>` Field names
#'
#' @autoglobal
#' @keywords internal
#' @export
fields_public <- function(url) {

  request(url) |>
    req_url_query(
      size   = 1,
      offset = 0) |>
    req_perform() |>
    resp_simple_json() |>
    _[["meta"]] |>
    _[["headers"]]

}

#' Request field names
#'
#' @param url `<chr>` API URL
#'
#' @returns `<chr>` Field names
#'
#' @autoglobal
#' @keywords internal
#' @export
fields_provider <- function(url) {

  request(url) |>
    req_url_query(
      limit   = 1,
      offset  = 0,
      count   = "false",
      schema  = "false",
      results = "false"
    ) |>
    req_perform() |>
    resp_simple_json() |>
    _[["query"]] |>
    _[["properties"]]
}
