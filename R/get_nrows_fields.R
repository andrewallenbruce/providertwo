#' Request number of results from public catalog
#' @param url `<chr>` API URL
#' @returns `<int>` Number of results
#' @autoglobal
#' @noRd
nrows_public <- function(url) {

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
nrows_provider <- function(url) {
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
fields_public <- function(url) {

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
fields_provider <- function(url) {

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
get_fields <- function(url) {
  if (sf_detect(url, "provider-data")) {
    fields_provider(url)
  } else {
    fields_public(url)
  }
}

#' Request total number of rows from catalog
#' @param url `<chr>` API URL
#' @returns `<chr>` Field names
#' @autoglobal
#' @noRd
get_nrows <- function(url) {
  if (sf_detect(url, "provider-data")) {
    nrows_provider(url)
  } else {
    nrows_public(url)
  }
}
