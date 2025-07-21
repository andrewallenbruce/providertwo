#' @autoglobal
#' @noRd
# get_dimensions2 <- new_generic("get_dimensions2", "obj", function(obj) {
#   S7_dispatch()
# })
#
# method(get_dimensions2, class_care) <- function(obj) {
#   prop(obj, "identifier") |>
#     get_dimensions2()
# }

#' @noRd
#' @autoglobal
get_dimensions <- function(x, call = caller_env()) {
  switch(
    x$catalog,
    care = switch(
      x$point,
      current  = dims_cur(x),
      temporal = dims_tmp(x),
      cli::cli_abort(c("x" = "{.val {x$point}} is invalid."), call = call)
      ),
    caid = dims(x, 8000L),
    prov = dims(x, 1500L),
    open = dims(x, 500L),
    hgov = dims(x, 500L),
    cli::cli_abort(c("x" = "{.val {x$catalog}} is invalid."), call = call)
  )
}

#' @autoglobal
#' @noRd
set_fields <- function(x) {
  set_names(new_list(length(x), NA_character_), x)
}

#' @autoglobal
#' @noRd
def_fields <- function(x) {
  new_list(1L, x %||% "An Unknown Error Has Occurred")
}

#' @autoglobal
#' @noRd
dims <- function(x, limit) {
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
dims_cur <- function(x) {
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
dims_tmp <- function(x) {
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
