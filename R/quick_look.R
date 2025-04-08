#' @autoglobal
#' @noRd
quick_look_pro <- function(x) {
  x |>
    request() |>
    req_url_query(
      count   = "false",
      format  = "json",
      keys    = "true",
      limit   = 2000L,
      offset  = 0L,
      results = "true",
      rowIds  = "false",
      schema  = "false"
    ) |>
    perform_simple() |>
    _[["results"]] |>
    map_na_if() |>
    as_tbl()
}

#' @autoglobal
#' @noRd
quick_care <- function(x) {

  x <- x |>
    request() |>
    req_url_query(
      offset = 0L,
      size   = 5000L) |>
    perform_simple()

  set_names(
    as_tbl(x$data),
    clean_names(x$meta$headers)) |>
    map_na_if()
}

#' @autoglobal
#' @noRd
quick_care_temp <- function(x) {
  x |>
    request() |>
    req_url_query(
      offset = 0L,
      size   = 5000L) |>
    perform_simple() |>
    map_na_if() |>
    as_tbl()
}

#' @autoglobal
#' @noRd
quick_open <- function(x) {
  x |>
    request() |>
    req_url_query(
      count   = "false",
      format  = "json",
      keys    = "true",
      limit   = 500L,
      offset  = 0L,
      results = "true",
      rowIds  = "false",
      schema  = "false"
    ) |>
    perform_simple() |>
    _[["results"]] |>
    map_na_if() |>
    as_tbl()
}

#' @autoglobal
#' @noRd
quick_open_temp <- function(x) {
  x |>
    open_url() |>
    request() |>
    req_url_query(
      count   = "false",
      format  = "json",
      keys    = "true",
      limit   = 500L,
      offset  = 0L,
      results = "true",
      rowIds  = "false",
      schema  = "false") |>
    perform_simple() |>
    _[["results"]] |>
    map_na_if() |>
    as_tbl()
}
