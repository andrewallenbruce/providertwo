#' @noRd
#' @autoglobal
get_dimensions <- function(x, call = caller_env()) {
  switch(
    x$clg,
    care = if (x$api == "end") care_end_dims(x) else care_tmp_dims(x),
    caid = def_dims(x, 8000L),
    prov = def_dims(x, 1500L),
    open = def_dims(x, 500L),
    hgov = def_dims(x, 500L),
    cli_abort(c("x" = "{.field clog} = {.val {x}} is invalid."), call = call)
  )
}

#' @autoglobal
#' @noRd
set_fields <- function(x) {
  set_names(new_list(length(x), character(0)), x)
}

#' @autoglobal
#' @noRd
def_fields <- function(x) {
  new_list(1L, x %||% "An Unknown Error Has Occurred")
}

#' @autoglobal
#' @noRd
def_dims <- function(x, limit) {
  x <- x$identifier |>
    request() |>
    req_error(is_error = ~ FALSE) |>
    perform_simple()

  class_dimensions(
    limit  = limit,
    rows   = x$count %||% 0L,
    fields = set_fields(x$query$properties) %||% def_fields(x$message)
  )
}

#' @autoglobal
#' @noRd
care_end_dims <- function(x) {
  x <- x$identifier |>
    request() |>
    req_error(is_error = ~ FALSE) |>
    perform_simple() |>
    get_elem("meta") |>
    get_elem(c("total_rows", "headers"))

  class_dimensions(
    limit  = 5000L,
    rows   = x$total_rows %||% 0L,
    fields = set_fields(x$headers) %||% def_fields(x$meta$message)
  )
}

#' @autoglobal
#' @noRd
care_tmp_dims <- function(x) {
  x <- x$identifier |>
    request() |>
    req_error(is_error = ~ FALSE)

  x <- list(
    headers = perform_simple(x) |> names(),
    total_rows = req_url_path_append(x, "stats") |>
      perform_simple() |>
      get_elem("total_rows")
  )
  class_dimensions(
    limit  = 5000L,
    rows   = x$total_rows %||% 0L,
    fields = set_fields(x$headers) %||% def_fields(x$meta$message)
  )
}
