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
dims_care_temp_group <- function(x, g) {

  reqs <- map(x, \(x) request(x) |> req_url_query(offset = 0L, size = 1L))

  fields_ <- req_perform_parallel(reqs, on_error = "continue") |>
    map(\(x) resp_simple_json(x) |> names()) |>
    set_names(g)

  rows_ <- map(reqs, \(x) req_url_path_append(x, "stats")) |>
    req_perform_parallel(on_error = "continue") |>
    map(\(x) resp_simple_json(x) |> _[["total_rows"]]) |>
    set_names(g)

  pages_ <- map(rows_, \(x) offset_size(x, 5000L)) |>
    set_names(g)

  list(
    rows   = rows_,
    fields = fields_,
    pages  = pages_
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
