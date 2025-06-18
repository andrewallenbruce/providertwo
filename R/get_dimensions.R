#' @noRd
#' @autoglobal
get_dimensions <- function(x, call = caller_env()) {
  switch(
    clog_(x),
    care = care_dims(x),
    caid = def_dims(x, 8000L),
    prov = def_dims(x, 1500L),
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
    req_url_query(splice(default_query(x))) |>
    req_error(is_error = ~ FALSE)
}

#' @autoglobal
#' @noRd
def_fields <- function(x) {
  new_list(1L, x %||% "An Unknown Error Has Occurred")
}

#' @autoglobal
#' @noRd
def_dims <- function(x, limit) {
  x <- req_dims(x) |> perform_simple()

  class_dimensions(
    limit  = limit,
    rows   = x$count %||% 0L,
    fields = x$query$properties %||% def_fields(x$message)
  )
}

#' @autoglobal
#' @noRd
set_fields <- function(x) {

  # x %||% return(def_fields(x$meta$message))

  new_list(
    length(x),
    character(0)) |>
    set_names(x)
}

#' @autoglobal
#' @noRd
care_dims <- function(x) {

  x <- switch(
    api_(x),
    end = req_dims(x) |>
      perform_simple() |>
      get_elem("meta") |>
      get_elem(c("total_rows", "headers")),
    tmp = list(
      headers = req_dims(x) |>
        perform_simple() |>
        names(),
      total_rows = req_dims(x) |>
        req_url_path_append("stats") |>
        perform_simple() |>
        get_elem("total_rows")
    )
  )
  class_dimensions2(
    limit  = 5000L,
    rows   = x$total_rows %||% 0L,
    fields = set_fields(x$headers) %||% def_fields(x$meta$message)
  )
}
