#' Request number of results for query
#' @param request `<httr_request>` API request
#' @returns `<int>` Number of results
#' @autoglobal
#' @noRd
query_nrows_public <- function(request) {

  req_url_path_append(
    request,
    "stats") |>
    req_perform() |>
    resp_simple_json() |>
    _[["data"]] |>
    _[["found_rows"]]

}

#' Parse JSON response
#' @param response `<httr_response>` API response
#' @returns `<tibble>` Parsed JSON response as tibble
#' @autoglobal
#' @noRd
parse_json_response <- function(response) {
  resp_body_string(response) |>
    fparse(query = "/data") |>
    as_tbl()

}

#' Mapped Parse JSON response
#' @param response `<httr_response>` API response
#' @returns `<tibble>` Parsed JSON response as tibble
#' @autoglobal
#' @noRd
map_parse_json_response <- function(response) {
  map(response, \(x) parse_json_response(x)) |>
    rowbind()
}

#' Clean JSON response
#' @param x `<data.frame>` tibble of API response
#' @param names `<chr>` vector of column names
#' @returns `<tibble>` Parsed JSON response as tibble
#' @autoglobal
#' @noRd
tidyup <- function(x, names) {
  set_names(
    x,
    names(names)) |>
    map_na_if()
}

#' Perform API request
#' @param url `<url>` API identifier url
#' @param query `<chr>` vector of query parameters
#' @param limit `<int>` API rate limit
#' @returns `<int>` Number of results
#' @autoglobal
#' @noRd
perform_request_public <- function(url, query, limit) {

  req <- request(url) |>
    req_url_query(
      !!!format_query(query),
      size = limit)

  n <- query_nrows_public(req)

  if (n == 0) {
    abort(
      message = c("!" = "0 results found."),
      use_cli_format = TRUE,
      call = caller_env(),
      class = "abort_no_results"
    )
  }

  nreq <- offset_length(n, limit) > 1

  if (false(nreq)) {
    return(
      req_perform(req) |>
      parse_json_response() |>
        tidyup(names = query)
      )
    } else {
      return(
        req_perform_iterative_offset(req, limit) |>
        map_parse_json_response() |>
        tidyup(names = query)
       )
  }

}
