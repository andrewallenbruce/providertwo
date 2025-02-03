#' Request number of results from public catalog
#' @param url `<chr>` API URL
#' @returns `<int>` Number of results
#' @autoglobal
#' @noRd
nrows_public <- \(url) {

  request(url) |>
    req_url_path_append("stats") |>
    req_perform() |>
    resp_body_json(
      simplifyVector = TRUE,
      check_type     = FALSE) |>
    _[["data"]] |>
    _[["found_rows"]]
}

#' Request number of results from provider catalog
#' @param url `<chr>` API URL
#' @returns `<int>` Number of results
#' @autoglobal
#' @noRd
nrows_provider <- \(url) {
  request(url) |>
    req_url_query(
      limit   = 1,
      offset  = 0,
      count   = "true",
      results = "false",
      schema  = "false") |>
    req_perform() |>
    resp_body_json(
      simplifyVector = TRUE,
      check_type     = FALSE) |>
    _[["count"]]
}

#' Request field names from public catalog
#' @param url `<chr>` API URL
#' @returns `<chr>` Field names
#' @autoglobal
#' @noRd
fields_public <- \(url) {

  request(url) |>
    req_url_query(
      size   = 1,
      offset = 0) |>
    req_perform() |>
    resp_body_json(
      simplifyVector = TRUE,
      check_type     = FALSE) |>
    _[["meta"]] |>
    _[["headers"]]

}

#' Request field names from provider catalog
#' @param url `<chr>` API URL
#' @returns `<chr>` Field names
#' @autoglobal
#' @noRd
fields_provider <- \(url) {

  request(url) |>
    req_url_query(
      limit  = 1,
      offset = 0) |>
    req_perform() |>
    resp_body_json(
      simplifyVector = TRUE,
      check_type     = FALSE) |>
    _[["query"]] |>
    _[["properties"]]

}

#' Request field names from catalog
#' @param url `<chr>` API URL
#' @returns `<chr>` Field names
#' @autoglobal
#' @noRd
get_fields <- \(url) {
  if (sf_detect(url, "provider-data")) {
    fields_provider(url)
  } else {
    fields_public(url)
  }
}

#' Request number of results
#' @param request `<httr_request>` API request
#' @returns `<int>` Number of results
#' @autoglobal
#' @noRd
request_nrows <- \(request) {

  req_url_path_append(
    request,
    "stats") |>
    req_perform() |>
    resp_body_json(
      simplifyVector = TRUE,
      check_type     = FALSE) |>
    _[["data"]] |>
    _[["found_rows"]]

}

#' Parse JSON response
#' @param response `<httr_response>` API response
#' @returns `<tibble>` Parsed JSON response as tibble
#' @autoglobal
#' @noRd
parse_json_response <- \(response) {
  resp_body_string(response) |>
    fparse(query = "/data") |>
    as_tbl()

}

#' Mapped Parse JSON response
#' @param response `<httr_response>` API response
#' @returns `<tibble>` Parsed JSON response as tibble
#' @autoglobal
#' @noRd
map_parse_json_response <- \(response) {
  map(response, \(x) parse_json_response(x)) |>
    rowbind()
}

#' Clean JSON response
#' @param x `<data.frame>` tibble of API response
#' @param names `<chr>` vector of column names
#' @returns `<tibble>` Parsed JSON response as tibble
#' @autoglobal
#' @noRd
tidyup <- \(x, names) {
  set_names(x, names(names)) |>
    map_na_if()
}

#' Perform API request
#' @param url `<url>` API identifier url
#' @param query `<chr>` vector of query parameters
#' @param limit `<int>` API rate limit
#' @returns `<int>` Number of results
#' @autoglobal
#' @noRd
perform_request <- \(url, query, limit) {

  req <- request(url) |>
    req_url_query(
      !!!format_query(query),
      size = limit)

  n <- request_nrows(req)

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
        req_perform_iterative(
          req,
          next_req      = iterate_with_offset(
          param_name    = "offset",
          start         = 0,
          offset        = limit,
          resp_complete = is_complete_with_limit(limit))) |>
        map_parse_json_response() |>
        tidyup(names = query)
       )
  }

}
