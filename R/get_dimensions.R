#' @noRd
#' @autoglobal
get_dimensions <- function(x, call = caller_env()) {
  switch(
    x$clog,
    care = care_dimensions(x),
    caid = caid_dimensions(x),
    prov = prov_dimensions(x),
    open = open_dimensions(x),
    hgov = hgov_dimensions(x),
    cli_abort(c("x" = "{.emph class} {.val {x}} is not a catalog type."), call = call)
  )
}

#' @autoglobal
#' @noRd
care_dimensions <- function(x, call = caller_env()) {
  req <- identifier_(x) |>
    request() |>
    req_url_query(offset = 0L, size = 1L) |>
    req_error(is_error = ~ FALSE)

  x <- switch(
    x$api,
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
    fields = x$headers %||% new_list(length = 1L, default = x$meta$message)
  )
}

#' @autoglobal
#' @noRd
caid_dimensions <- function(x, call = caller_env()) {
  x <- identifier_(x) |>
    request() |>
    req_url_query(offset = 0L,
                  limit = 1L,
                  results = "false") |>
    req_error(is_error = ~ FALSE) |>
    perform_simple()

  class_dimensions(
    limit  = 8000L,
    rows   = x$count %||% 0L,
    fields = x$query$properties %||% new_list(length = 1L, default = x$message)
  )
}

#' @autoglobal
#' @noRd
prov_dimensions <- function(x, call = caller_env()) {
  x <- identifier_(x) |>
    request() |>
    req_url_query(offset = 0L,
                  limit = 1L,
                  results = "false") |>
    req_error(is_error = ~ FALSE) |>
    perform_simple()

  class_dimensions(
    limit  = 2000L,
    rows   = x$count %||% 0L,
    fields = x$query$properties %||% new_list(length = 1L, default = x$message)
  )
}

#' @autoglobal
#' @noRd
open_dimensions <- function(x, call = caller_env()) {
  x <- identifier_(x) |>
    request() |>
    req_url_query(offset = 0L,
                  limit = 1L,
                  results = "false") |>
    req_error(is_error = ~ FALSE) |>
    perform_simple()

  class_dimensions(
    limit  = 500L,
    rows   = x$count %||% 0L,
    fields = x$query$properties %||% new_list(length = 1L, default = x$message)
  )
}

#' @autoglobal
#' @noRd
hgov_dimensions <- function(x, call = caller_env()) {
  x <- identifier_(x) |>
    request() |>
    req_url_query(offset = 0L,
                  limit = 1L,
                  results = "false") |>
    req_error(is_error = ~ FALSE) |>
    perform_simple()

  class_dimensions(
    limit  = 500L,
    rows   = x$count %||% 0L,
    fields = x$query$properties %||% new_list(length = 1L, default = x$message)
  )
}
