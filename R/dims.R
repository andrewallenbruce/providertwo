#' @autoglobal
#' @noRd
dims_open <- function(x) {
  x <- x |>
    request() |>
    req_url_query(
      schema  = "false",
      keys    = "false",
      results = "false",
      count   = "true",
      format  = "json",
      rowIds  = "false",
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
dims_caid <- function(x) {

  x <- x |>
    request() |>
    req_url_query(
      schema  = "false",
      keys    = "false",
      results = "false",
      count   = "true",
      format  = "json",
      rowIds  = "false",
      offset  = 0L,
      limit   = 1L
    ) |>
    perform_simple()

  list_tidy(
    rows   = x$count,
    fields = x$query$properties,
    pages  = offset_size(rows, 8000L))

}
