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
#' @returns `<int>` Number of results
#' @autoglobal
#' @keywords internal
#' @export
perform_request_public <- function(url, query) {

  req <- req_url_query(
    request(url),
    !!!format_query_public(query))

  n <- query_nrows_public(req)

  if (n == 0) {
    cli::cli_abort(
      message = c(
        "x" = "{n} result{?s} found.",
        " " = " "),
      call = caller_env(),
      class = "abort_no_results")
  }

  nreq <- offset_length(n, query$size) > 1

  cli_n_results_requests(n, query$size)

  if (false(nreq)) {
    return(
      req_perform(req) |>
      parse_json_response_public()
      )
    } else {
      return(
        req_perform_iterative_offset(req, query$size) |>
        map_parse_json_response_public()
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
    cli::cli_abort(
      message = c(
        "x" = "{n} result{?s} found.",
        " " = " "),
      call = caller_env(),
      class = "abort_no_results")
  }

  nreq <- offset_length(n, limit) > 1

  cli_n_results_requests(n, limit)

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

#' Inform number of results and requests
#'
#' @param n     `<int>` Number of results returned in an API request
#'
#' @param limit `<int>` API rate limit, i.e. the maximum number of results an
#'                      API will return per request.
#' @returns cli message
#' @autoglobal
#' @keywords internal
#' @export
cli_n_results_requests <- function(n, limit) {

  r   <- offset_length(n, limit)
  res <- ifelse(n > 1, "Results", "Result")
  req <- ifelse(r > 1, "Requests", "Request")

  res <- cli::col_cyan(res)
  req <- cli::col_cyan(req)

  r <- cli::style_bold(cli::col_yellow(prettyNum(r, big.mark = ",")))
  n <- cli::style_bold(cli::col_yellow(prettyNum(n, big.mark = ",")))
  m <- cli::col_silver(cli::symbol$menu)


  cli::cli_inform(
    c("i" = "{n} {res} {m} {r} {req}",
      " " = " "))
}
