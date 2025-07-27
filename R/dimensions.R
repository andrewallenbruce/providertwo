#' @autoglobal
#' @noRd
url_type <- function(x) {
  api <- case(
    grepl("data.cms.gov/provider-data", x, perl = TRUE) ~ "prov",
    grepl("openpaymentsdata.cms.gov", x, perl = TRUE)   ~ "open",
    grepl("data.medicaid.gov", x, perl = TRUE)          ~ "caid",
    grepl("data.healthcare.gov", x, perl = TRUE)        ~ "hgov",
    grepl("data.cms.gov/data-api", x, perl = TRUE)      ~ "care",
    .default = NA_character_
  )

  if (is.na(api))
    cli::cli_abort(c("x" = "{.val {x}} could not be categorized."))

  if (api != "care")
    return(api)

  case(
    grepl("/data-viewer?", x, perl = TRUE) ~ "current",
    grepl("/data?", x, perl = TRUE)        ~ "temporal"
  )
}

#' @autoglobal
#' @noRd
dims_default <- function(x, limit) {
  if (url_type(x$identifier) %!in_% c("current", "temporal"))

    id <- x$identifier |>
      request() |>
      req_error(is_error = ~ FALSE) |>
      perform_simple()

  class_dimensions(
    limit  = switch(
      url_type(x$identifier),
      caid = 8000L,
      prov = 1500L,
      open = ,
      hgov = 500L
    ),
    rows   = id$count %||% 0L,
    fields = id$query$properties %||% character(0)
  )
}

#' @autoglobal
#' @noRd
dims_care <- function(x) {
  id <- x$identifier |>
    request() |>
    req_error(is_error = ~ FALSE)

  x <- switch(
    url_type(x$identifier),
    current      = perform_simple(id) |>
      get_elem("meta") |>
      get_elem(c("total_rows", "headers")),
    temporal     = list(
      headers    = names(perform_simple(id)),
      total_rows = req_url_path_append(id, "stats") |>
        perform_simple() |>
        get_elem("total_rows")
    )
  )

  class_dimensions(
    limit  = 5000L,
    rows   = x$total_rows %||% 0L,
    fields = x$headers %||% character(0)
  )
}

# dims_tmp <- function(x) {
#   x <- x$identifier |>
#     request() |>
#     req_error(is_error = ~ FALSE)
#
#   x <- list(
#     headers = perform_simple(x) |> names(),
#     total_rows = req_url_path_append(x, "stats") |>
#       perform_simple() |>
#       get_elem("total_rows")
#   )
#   class_dimensions(
#     limit  = 5000L,
#     rows   = x$total_rows %||% 0L,
#     fields = set_fields(x$headers) %||% def_fields(x$meta$message)
#   )
# }
#
# get_dimensions <- function(x, call = caller_env()) {
#   switch(
#     x$catalog,
#     care = switch(
#       x$point,
#       current  = dims_cur(x),
#       temporal = dims_tmp(x),
#       cli::cli_abort(c("x" = "{.val {x$point}} is invalid."), call = call)
#       ),
#     caid = dims(x, 8000L),
#     prov = dims(x, 1500L),
#     open = dims(x, 500L),
#     hgov = dims(x, 500L),
#     cli::cli_abort(c("x" = "{.val {x$catalog}} is invalid."), call = call)
#   )
# }
#
#
# set_fields <- function(x) {
#   set_names(new_list(length(x), NA_character_), x)
# }
#
# def_fields <- function(x) {
#   new_list(1L, x %||% "An Unknown Error Has Occurred")
# }
