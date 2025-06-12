#' @noRd
#' @autoglobal
get_dimensions <- function(x, call = caller_env()) {
  switch(
    clog_(x),
    care = care_dims(x),
    caid = def_dims(x, 8000L),
    prov = def_dims(x, 2000L),
    open = def_dims(x, 500L),
    hgov = def_dims(x, 500L),
    cli_abort(c("x" = "{.emph class} {.val {x}} is not a catalog type."), call = call)
  )
}

#' @autoglobal
#' @noRd
req_dims <- function(x) {
  identifier_(x) |>
    request() |>
    req_url_query(offset = 0L, size = 1L) |>
    req_error(is_error = ~ FALSE)
}

#' @autoglobal
#' @noRd
def_fields <- function(x) {
  new_list(1L, x %||% "An Unknown Error Has Occurred")
}

#' @autoglobal
#' @noRd
def_dims <- function(x, limit, call = caller_env()) {
  x <- req_dims(x) |> perform_simple()

  class_dimensions(
    limit  = limit,
    rows   = x$count %||% 0L,
    fields = x$query$properties %||% def_fields(x$message)
  )
}


#' @autoglobal
#' @noRd
care_dims <- function(x, req = req_dims(x), call = caller_env()) {

  x <- switch(
    api_(x),
    end = perform_simple(req) |>
      get_elem("meta") |>
      get_elem(c("total_rows", "headers")),
    tmp = list(
      headers = perform_simple(req) |> names(),
      total_rows = req_url_path_append(req, "stats") |>
        perform_simple() |>
        get_elem("total_rows")
    )
  )
  class_dimensions(
    limit  = 5000L,
    rows   = x$total_rows %||% 0L,
    fields = x$headers %||% def_fields(x$meta$message)
  )
}
