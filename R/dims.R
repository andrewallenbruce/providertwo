#' @autoglobal
#' @noRd
dims_main <- function(url) {
  x <- url |>
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
dims_main_temp <- function(url) {

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
    rows   = get_rows(url),
    fields = get_fields(url),
    pages  = offset_size(rows, 5000L)
  )
}

#' @autoglobal
#' @noRd
dims_pro <- function(uuid) {
  x <- uuid |>
    pro_url() |>
    request() |>
    req_url_query(
      schema  = "false",
      keys    = "false",
      results = "false",
      count   = "true",
      offset  = 0,
      limit   = 1
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
dims_open <- function(uuid) {
  x <- open_url(uuid) |>
    request() |>
    req_url_query(
      schema  = "false",
      keys    = "false",
      results = "false",
      count   = "true",
      offset  = 0,
      limit   = 1
    ) |>
    perform_simple()

  list_tidy(
    rows   = x$count,
    fields = x$query$properties,
    pages  = offset_size(rows, 5000L)
  )
}
