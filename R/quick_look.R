#' @autoglobal
#' @noRd
cclean <- function(x) {
  gsub("\\(|\\)", "", clean_names(x))
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

# quick_care_temp(careTemp("quality_payment")@endpoints$identifier[1])
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
    rnm(cclean) |>
    as_tbl()
}

# quick_pro(proMain("clinicians")@identifier)
#' @autoglobal
#' @noRd
quick_pro <- function(x) {
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
      limit   = 2000L
    ) |>
    perform_simple() |>
    _[["results"]] |>
    map_na_if() |>
    rnm(cclean) |>
    as_tbl()
}

# quick_open(openMain("profile_covered")@identifier) |> str()
#' @autoglobal
#' @noRd
quick_open <- function(x) {
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
      limit   = 500L
    ) |>
    perform_simple() |>
    _[["results"]] |>
    map_na_if() |>
    rnm(cclean) |>
    as_tbl()
}

# quick_open_temp(prop(openTemp("ownership"), "endpoints")$identifier[1])
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
      results = "true",
      rowIds  = "false",
      schema  = "false",
      offset  = 0L,
      limit   = 500L
      ) |>
    perform_simple() |>
    _[["results"]] |>
    map_na_if() |>
    rnm(cclean) |>
    as_tbl()
}

quick_caid <- function(x) {
  x |>
    caid_url() |>
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
    rnm(cclean) |>
    as_tbl()
}
