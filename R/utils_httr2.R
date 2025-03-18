#' Parse Simple JSON Response
#' @param resp `<httr2_response>` object
#' @returns `<list>` Parsed JSON response
#' @autoglobal
#' @keywords internal
#' @export
resp_simple_json <- function(resp, ...) {
  resp_body_json(
    resp,
    simplifyVector = TRUE,
    check_type     = FALSE,
    ...)
}

#' Perform and Parse Simple JSON Response
#' @param x `<httr2_request>` object
#' @returns `<list>` Parsed JSON response
#' @autoglobal
#' @keywords internal
#' @export
perform_simple <- function(x) {
  resp_simple_json(req_perform(x))
}

#' Helper for `iterate_with_offset`
#' @param limit `<int>` API rate limit, i.e. the maximum number of results an
#'                      API will return per request.
#' @returns `<function>` Function to check if API request is complete
#' @autoglobal
#' @keywords internal
#' @export
is_complete_with_limit <- function(limit) {

  # TODO: allow switching between different API limits?
  check_number_whole(limit, min = 1, max = 5000)

  function(resp) length(resp_body_json(resp)$data) < limit

}

#' Perform Iterative API request with Offset
#' @param req `<httr2_request>` object
#' @param limit `<int>` API rate limit, i.e. the maximum number of results an
#'                      API will return per request.
#' @returns list of `<httr2_response>`s
#' @keywords internal
#' @export
req_perform_iterative_offset <- function(req, limit) {

  req_perform_iterative(
    req,
    next_req        = iterate_with_offset(
      param_name    = "offset",
      start         = 0,
      offset        = limit,
      resp_complete = is_complete_with_limit(limit)
      )
    )
}

#' Parse JSON response
#' @param response `<httr_response>` API response
#' @returns `<tibble>` Parsed JSON response as tibble
#' @autoglobal
#' @keywords internal
#' @export
parse_json_response_public <- function(response) {
  resp_body_string(response) |>
    fparse(query = "/data") |>
    as_tbl()
}

#' Parse JSON response
#' @param response `<httr_response>` API response
#' @returns `<tibble>` Parsed JSON response as tibble
#' @autoglobal
#' @keywords internal
#' @export
parse_json_response_provider <- function(response) {
  resp_body_string(response) |>
    fparse(query = "/results") |>
    as_tbl()
}

#' Mapped Parse JSON response
#' @param response `<httr_response>` API response
#' @returns `<tibble>` Parsed JSON response as tibble
#' @autoglobal
#' @keywords internal
#' @export
map_parse_json_response_public <- function(response) {
  map(response, function(x)
    parse_json_response_public(x)) |>
    rowbind()
}

#' Mapped Parse JSON response
#' @param response `<httr_response>` API response
#' @returns `<tibble>` Parsed JSON response as tibble
#' @autoglobal
#' @keywords internal
#' @export
map_parse_json_response_provider <- function(response) {
  map(response, function(x)
    parse_json_response_provider(x)) |>
    rowbind()
}

#' Clean JSON response
#' @param x `<data.frame>` tibble of API response
#' @param names `<chr>` vector of column names
#' @returns `<tibble>` Parsed JSON response as tibble
#' @autoglobal
#' @keywords internal
#' @export
tidyup <- function(x, names) {
  set_names(
    x,
    names(names)) |>
    map_na_if()
}

#' Request number of results from public catalog
#' @param url `<chr>` API URL
#' @returns `<int>` Number of results
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

#' Request number of results from provider catalog
#' @param url `<chr>` API URL
#' @returns `<int>` Number of results
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

#' Request total number of rows from catalog
#' @param url `<chr>` API URL
#' @returns `<chr>` Field names
#' @autoglobal
#' @keywords internal
#' @export
get_nrows <- function(url) {
  if (sf_detect(url, "provider-data|openpaymentsdata")) {
    nrows_provider(url)
  } else {
    nrows_public(url)
  }
}

#' Request field names from public catalog
#' @param url `<chr>` API URL
#' @returns `<chr>` Field names
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

#' Request field names from provider catalog
#' @param url `<chr>` API URL
#' @returns `<chr>` Field names
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

#' Request field names from catalog
#' @param url `<chr>` API URL
#' @returns `<chr>` Field names
#' @autoglobal
#' @keywords internal
#' @export
get_fields <- function(url) {
  if (sf_detect(url, "provider-data|openpaymentsdata")) {
    fields_provider(url)
  } else {
    fields_public(url)
  }
}

#' Get Resources
#' @param url resourcesAPI url
#' @returns `<tibble>` of Resource Files Data
#' @autoglobal
#' @keywords internal
#' @export
get_resources <- function(url) {

  if (not_na(url)) {

    fload(url, query = "/data") |>
      as_tbl() |>
      fcompute(file        = name,
               size        = roundup(fileSize / 1e6),
               ext         = file_ext(downloadURL),
               downloadurl = downloadURL) |>
      roworder(ext, -size)
  }
}

