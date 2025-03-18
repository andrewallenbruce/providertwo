#' Request number of results from Public Query
#' @param req `<httr_request>` API request
#' @returns `<int>` Number of results
#' @autoglobal
#' @keywords internal
#' @export
query_nrows_public <- function(req) {

  req_url_path_append(req, "stats") |>
    perform_simple() |>
    _[["data"]] |>
    _[["found_rows"]]

}

#' Request number of results from Provider Query
#' @param req `<httr_request>` API request
#' @returns `<int>` Number of results
#' @autoglobal
#' @keywords internal
#' @export
query_nrows_provider <- function(req) {

    req_url_query(
      req,
      limit   = 1,
      offset  = 0,
      count   = "true",
      results = "false",
      schema  = "false") |>
    perform_simple() |>
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

  req <- req_url_query(request(url),
    !!!format_query_public(query),
    size = 5000L)

  n <- query_nrows_public(req)

  if (n == 0) {
    cli::cli_abort(
      message = c("x" = "{n} result{?s} found.", " " = " "),
      call = caller_env(),
      class = "abort_no_results"
    )
  }

  nreq <- offset_length(n, 5000L) > 1

  cli_results(n, 5000L)

  if (false(nreq)) {
    return(
      req_perform(req) |>
      parse_json_response_public() |>
        tidyup(names = query)
      )
    } else {
      return(
        req_perform_iterative_offset(req, 5000L) |>
        map_parse_json_response_public() |>
          tidyup(names = query)
       )
  }

}

#' Perform Provider API request
#' @param url `<url>` API identifier url
#' @param query `<chr>` vector of query parameters
#' @returns `<int>` Number of results
#' @autoglobal
#' @keywords internal
#' @export
perform_request_provider <- function(url, query) {

  req <- req_url_query(request(url),
    !!!format_query_provider(query),
    limit = 2000L)

  n <- query_nrows_provider(req)


  if (n == 0) {
    cli::cli_abort(
      message = c(
        "x" = "{n} result{?s} found.",
        " " = " "),
      call = caller_env(),
      class = "abort_no_results")
  }

  nreq <- offset_length(n, 2000L) > 1

  cli_results(n, 2000L)

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
