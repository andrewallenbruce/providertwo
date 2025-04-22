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
    pages  = offset_size(x$total_rows, 5000L)
  )
}

#' @autoglobal
#' @noRd
dims_care_temp <- function(x) {

  x <- x |>
    request() |>
    req_url_query(
      offset = 0L,
      size   = 1L)

  list_tidy(
    rows   = x |> req_url_path_append("stats") |> perform_simple() |> _[["total_rows"]],
    fields = x |> perform_simple() |> names(),
    pages  = offset_size(rows, 5000L)
  )
}

#' @autoglobal
#' @noRd
dims_care_temp_group <- function(x, g) {

  reqs <- map(x, \(x) request(x) |>
                req_url_query(offset = 0L,
                              size = 1L))

  FLD <- req_perform_parallel(reqs, on_error = "continue") |>
    map(\(x) names(resp_simple_json(x)))

  reqs <- map(reqs, \(x) req_url_path_append(x, "stats"))

  ROW <- req_perform_parallel(reqs, on_error = "continue") |>
    map(\(x) resp_simple_json(x) |> _[["total_rows"]])

  list(
    rows   = set_names(ROW, g),
    fields = set_names(FLD, g),
    pages  = map(ROW, \(x) offset_size(x, 5000L)) |> set_names(g)
  )
}

#' @autoglobal
#' @noRd
dims_pro <- function(x) {
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
    pages  = offset_size(rows, 2000L)
  )
}

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
