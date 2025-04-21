# quick_care("enrollees")
#' @autoglobal
#' @noRd
quick_care <- function(x, offset = 0L) {
  x <- careMain(alias = x) |>
    prop("identifier") |>
    request() |>
    req_url_query(offset = offset, size = 5000L) |>
    perform_simple()

  x$data |>
    as_tbl() |>
    set_clean(x$meta$headers) |>
    map_na_if()
}

# quick_care_temp("quality_payment")
#' @autoglobal
#' @noRd
quick_care_temp <- function(x, offset = 0L) {
  careTemp(alias = x) |>
    prop("endpoints") |>
    _[["identifier"]][1] |>
    request() |>
    req_url_query(offset = offset, size = 5000L) |>
    perform_simple() |>
    map_na_if() |>
    rnm(clean_names) |>
    as_tbl()
}

# quick_pro("clinicians")
#' @autoglobal
#' @noRd
quick_pro <- function(x, offset = 0L) {
  proMain(alias = x) |>
    prop("identifier") |>
    request() |>
    req_url_query(
      count   = "false",
      format  = "json",
      keys    = "true",
      results = "true",
      rowIds  = "false",
      schema  = "false",
      offset  = offset,
      limit   = 2000L
    ) |>
    perform_simple() |>
    _[["results"]] |>
    map_na_if() |>
    rnm(clean_names) |>
    as_tbl()
}

# quick_open("profile_covered")
#' @autoglobal
#' @noRd
quick_open <- function(x, offset = 0L) {
  openMain(alias = x) |>
    prop("identifier") |>
    request() |>
    req_url_query(
      count   = "false",
      format  = "json",
      keys    = "true",
      results = "true",
      rowIds  = "false",
      schema  = "false",
      offset  = offset,
      limit   = 500L
    ) |>
    perform_simple() |>
    _[["results"]] |>
    map_na_if() |>
    rnm(clean_names) |>
    as_tbl()
}

# quick_open_temp("ownership")
#' @autoglobal
#' @noRd
quick_open_temp <- function(x, offset = 0L) {
  openTemp(alias = x) |>
    prop("endpoints") |>
    _[["identifier"]][1] |>
    request() |>
    req_url_query(
      count   = "false",
      format  = "json",
      keys    = "true",
      results = "true",
      rowIds  = "false",
      schema  = "false",
      offset  = offset,
      limit   = 500L
    ) |>
    perform_simple() |>
    _[["results"]] |>
    map_na_if() |>
    rnm(clean_names) |>
    as_tbl()
}

quick_caid <- function(x) {
  x |>
    request() |>
    req_url_query(
      count   = "false",
      format  = "json",
      keys    = "true",
      results = "true",
      rowIds  = "false",
      schema  = "false",
      offset  = 0L,
      limit   = 8000L
    ) |>
    perform_simple() |>
    _[["results"]] |>
    map_na_if() |>
    rnm(clean_names) |>
    as_tbl()
}
