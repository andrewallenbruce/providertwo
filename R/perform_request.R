#' Request number of results from Public Query
#' @param request `<httr_request>` API request
#' @returns `<int>` Number of results
#' @autoglobal
#' @keywords internal
#' @export
query_nrows_public <- function(request) {

  req_url_path_append(request, "stats") |>
    req_perform() |>
    resp_simple_json() |>
    _[["data"]] |>
    _[["found_rows"]]

}

#' Request number of results from Provider Query
#' @param request `<httr_request>` API request
#' @returns `<int>` Number of results
#' @autoglobal
#' @keywords internal
#' @export
query_nrows_provider <- function(request) {

    req_url_query(
      request,
      limit   = 1,
      offset  = 0,
      count   = "true",
      results = "false",
      schema  = "false") |>
    req_perform() |>
    resp_simple_json() |>
    _[["count"]]
}

#' Perform Public API request
#' @param url `<url>` API identifier url
#' @param query `<chr>` vector of query parameters
#' @param limit `<int>` API rate limit
#' @returns `<int>` Number of results
#' @autoglobal
#' @keywords internal
#' @export
perform_request_public <- function(url, query, limit) {

  req <- req_url_query(
    request(url),
    !!!format_query_public(query),
    size = limit)

  n <- query_nrows_public(req)

  if (n == 0) {
    abort(
      message        = c("!" = "0 results found."),
      use_cli_format = TRUE,
      call           = caller_env(),
      class          = "abort_no_results"
    )
  }

  nreq <- offset_length(n, limit) > 1

  if (false(nreq)) {
    return(
      req_perform(req) |>
      parse_json_response_public() |>
        tidyup(names = query)
      )
    } else {
      return(
        req_perform_iterative_offset(req, limit) |>
        map_parse_json_response_public() |>
        tidyup(names = query)
       )
  }

}

#' Perform Provider API request
#' @param url `<url>` API identifier url
#' @param query `<chr>` vector of query parameters
#' @param limit `<int>` API rate limit
#' @returns `<int>` Number of results
#' @autoglobal
#' @keywords internal
#' @export
perform_request_provider <- function(url, query, limit) {

  req <- req_url_query(
    request(url),
    !!!format_query_provider(query),
    limit = limit
    )

  n <- query_nrows_provider(req)


  if (n == 0) {
    abort(
      message = c("x" = "0 results found."),
      use_cli_format = TRUE,
      call = caller_env(),
      class = "abort_no_results"
    )
  }

  nreq <- offset_length(n, limit) > 1

  cli::cli_inform(
    c("i" = "{n} result{?s} found.",
      "!" = "{offset_length(n, limit)} request{?s} will be made.",
      " " = " "))

  if (false(nreq)) {
    return(
      req_perform(req) |>
        parse_json_response_provider() |>
        tidyup(names = query)
    )
  } else {
    return(
      req_perform_iterative_offset(req, limit) |>
        map_parse_json_response_provider() |>
        tidyup(names = query)
    )
  }

}
