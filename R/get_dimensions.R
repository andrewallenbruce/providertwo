#' @noRd
#' @autoglobal
get_metadata <- function(x) {
  compact(list(
    title       = null_if(x$title),
    description = null_if(x$description),
    modified    = null_if(x$modified),
    group       = null_if(x$group),
    issued      = null_if(x$issued),
    released    = null_if(x$released),
    temporal    = null_if(x$temporal),
    periodicity = null_if(x$periodicity),
    download    = null_if(x$download),
    resources   = unlist_if(null_if(x$resources)),
    dictionary  = null_if(x$dictionary),
    site        = null_if(x$site),
    references  = null_if(x$references)
  ))
}

#' @noRd
#' @autoglobal
get_dimensions <- function(x, clog, api = NULL, call = caller_env()) {

  if (clog == "care" && is.null(api)) {
    cli_abort(
      c("x" = "{.field clog} = {.val care} requires {.field api} arg."),
      call = call)
  }

  switch(
    clog,
    care = ifelse(api == "end", care_end_dims(x), care_tmp_dims(x)),
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
