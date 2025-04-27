#' @autoglobal
#' @noRd
convert_epoch <- function(x) {
  as.Date(as.POSIXct.numeric(as.numeric(x) / 1000L, origin = "1970-01-01"))
}

#' @noRd
yank <- function(x) x[[1]]

#' @noRd
yank_index_name <- function(x, nm, i = 1L) get_elem(x[[i]], elem = rlang::ensym(nm))


# quick_care_temp("quality_payment")
#' @autoglobal
#' @noRd
quick_care_temp <- function(x, offset = 0L, limit = 5000L) {
  careTemp(x) |>
    prop("endpoints") |>
    _[["identifier"]][1] |>
    request() |>
    req_url_query(
      offset = thresh(offset, total_rows(careTemp(x))),
      size   = thresh(limit, 5000L)
    ) |>
    perform_simple() |>
    map_na_if() |>
    rnm(clean_names) |>
    as_tbl()
}

# quick_open("profile_covered")
#' @autoglobal
#' @noRd
quick_open <- function(x, offset = 0L, limit = 500L) {
  openMain(x) |>
    prop("identifier") |>
    request() |>
    req_url_query(
      count   = "false",
      format  = "json",
      keys    = "true",
      results = "true",
      rowIds  = "false",
      schema  = "false",
      offset  = thresh(offset, total_rows(openMain(x))),
      limit   = thresh(limit, 500L)
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
quick_open_temp <- function(x, offset = 0L, limit = 500L) {
  openTemp(x) |>
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
      offset  = thresh(offset, total_rows(openTemp(x))),
      limit   = thresh(limit, 500L)
    ) |>
    perform_simple() |>
    _[["results"]] |>
    map_na_if() |>
    rnm(clean_names) |>
    as_tbl()
}
