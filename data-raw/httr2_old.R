#' @autoglobal
#' @noRd
is_complete_with_limit <- function(limit) {
  function(resp)
    length(resp_body_json(resp)$data) < limit
}

#' @autoglobal
#' @noRd
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

#' @autoglobal
#' @noRd
parse_json_response_public <- function(resp) {
  resp_body_string(resp) |>
    fparse(query = "/data") |>
    as_tbl()
}

#' @autoglobal
#' @noRd
parse_json_response_provider <- function(resp) {
  resp_body_string(resp) |>
    fparse(query = "/results") |>
    as_tbl()
}

#' @autoglobal
#' @noRd
map_parse_json_response_public <- function(resp) {
  map(resp, function(x)
    parse_json_response_public(x)) |>
    rowbind()
}

#' @autoglobal
#' @noRd
map_parse_json_response_provider <- function(resp) {
  map(resp, function(x)
    parse_json_response_provider(x)) |>
    rowbind()
}

#' @autoglobal
#' @noRd
tidyup2 <- function(x, names) {
  set_names(x, names(names)) |>
    map_na_if()
}

#' @autoglobal
#' @noRd
nrows_public <- function(url) {
  request(url) |>
    req_url_path_append("stats") |>
    perform_simple() |>
    _[["data"]] |>
    _[["found_rows"]]
}

#' @autoglobal
#' @noRd
nrows_provider <- function(url) {
  request(url) |>
    req_url_query(
      limit   = 1,
      offset  = 0,
      count   = "true",
      results = "false",
      schema  = "false"
    ) |>
    perform_simple() |>
    _[["count"]]
}

#' @autoglobal
#' @noRd
fields_public <- function(url) {
  request(url) |>
    req_url_query(size   = 1, offset = 0) |>
    perform_simple() |>
    _[["meta"]] |>
    _[["headers"]]

}

#' @autoglobal
#' @noRd
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

#' @autoglobal
#' @noRd
query_nrows_public <- function(req) {
  req_url_path_append(req, "stats") |>
    perform_simple() |>
    _[["data"]] |>
    _[["found_rows"]]

}

#' @autoglobal
#' @noRd
query_nrows_provider <- function(req) {
  req_url_query(
    req,
    limit   = 1,
    offset  = 0,
    count   = "true",
    results = "false",
    schema  = "false"
  ) |>
    perform_simple() |>
    _[["count"]]
}

#' @autoglobal
#' @noRd
perform_request_public <- function(url, query) {

  req <- url |>
    request() |>
    req_url_query(
      !!!format_query_public(query),
      size = 5000L)

  n <- query_nrows_public(req)

  if (n == 0) {
    cli_abort(
      c("x" = "No results found.",
        " " = " "),
      call = caller_env())
  }

  nreq <- offset_size(n, 5000L) > 1

  cli_results(n, 5000L)

  if (false(nreq)) {
    return(
      req_perform(req) |>
        parse_json_response_public() |>
        tidyup2(names = query)
    )
  } else {
    return(
      req_perform_iterative_offset(req, 5000L) |>
        map_parse_json_response_public() |>
        tidyup2(names = query)
    )
  }

}

#' @autoglobal
#' @noRd
perform_request_provider <- function(url, query) {

  req <- url |>
    request() |>
    req_url_query(
      !!!format_query_provider(query),
      limit = 2000L)

  n <- query_nrows_provider(req)

  if (n == 0) {
    cli_abort(
      c("x" = "No results found.",
        " " = " "),
      call = caller_env())
  }

  nreq <- offset_size(n, 2000L) > 1

  cli_results(n, 2000L)

  if (false(nreq)) {
    return(
      req_perform(req) |>
        parse_json_response_provider() |>
        tidyup2(names = query)
    )
  } else {
    return(
      req_perform_iterative_offset(req, limit) |>
        map_parse_json_response_provider() |>
        tidyup2(names = query)
    )
  }
}
