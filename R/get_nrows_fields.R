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

# get_resources <- function(url) {
#   if (not_na(url)) {
#     as_tbl(fload(url, query = "/data")) |>
#       mtt(fileSize = trimws(as_chr(parse_bytes(
#         as_chr(fileSize)
#       ))),
#       fileType = file_ext(downloadURL)) |>
#       colorder(downloadURL, pos = "end")
#   }
# }
