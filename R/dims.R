#' @autoglobal
#' @noRd
dims_care <- function(x) {
  x <- x |>
    request() |>
    req_url_query(offset = 0L, size = 1L) |>
    perform_simple() |>
    _[["meta"]]

  list_tidy(
    rows   = x$total_rows,
    fields = x$headers,
    pages  = offset_size(rows, 5000L)
  )
}

#' @autoglobal
#' @noRd
dims_care_temp <- function(x) {

  get_rows <- \(x) {
    request(x) |>
      req_url_path_append("stats") |>
      perform_simple() |>
      _[["total_rows"]]
    }

  get_fields <- \(x) {
    request(x) |>
      req_url_query(offset = 0L, size = 1L) |>
      perform_simple() |>
      names()
    }

  list_tidy(
    rows   = get_rows(x),
    fields = get_fields(x),
    pages  = offset_size(rows, 5000L)
  )
}

#' @autoglobal
#' @noRd
dims_pro <- function(x) {
  x <- x |>
    pro_url() |>
    request() |>
    req_url_query(
      schema  = "false",
      keys    = "false",
      results = "false",
      count   = "true",
      offset  = 0L,
      limit   = 1L
    ) |>
    perform_simple()

  list_tidy(
    rows   = x$count,
    fields = x$query$properties,
    pages  = offset_size(rows, 2000L)
  )
}

#' @autoglobal
#' @noRd
dims_open <- function(x) {
  x <- x |>
    open_url() |>
    request() |>
    req_url_query(
      schema  = "false",
      keys    = "false",
      results = "false",
      count   = "true",
      offset  = 0L,
      limit   = 1L
    ) |>
    perform_simple()

  list_tidy(
    rows   = x$count,
    fields = x$query$properties,
    pages  = offset_size(rows, 500L)
  )
}

#' @autoglobal
#' @noRd
dims_caid <- function(uuid) {

  x <- uuid |>
    caid_url() |>
    request() |>
    req_url_query(
      schema  = "false",
      keys    = "false",
      results = "true",
      count   = "true",
      format  = "json",
      rowIds  = "false",
      limit   = 1,
      offset  = 0
    ) |>
    perform_simple()

  list_tidy(
    rows   = x$count,
    fields = x$query$properties,
    pages  = offset_size(rows, 8000L))

}
