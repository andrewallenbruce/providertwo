#' @autoglobal
#' @noRd
url_type <- function(x) {
  api <- case(
    grepl("data.cms.gov/provider-data", x, perl = TRUE) ~ "prov",
    grepl("openpaymentsdata.cms.gov",   x, perl = TRUE) ~ "open",
    grepl("data.medicaid.gov",          x, perl = TRUE) ~ "caid",
    grepl("data.healthcare.gov",        x, perl = TRUE) ~ "hgov",
    grepl("data.cms.gov/data-api",      x, perl = TRUE) ~ "care",
    .default = NA_character_
  )

  if (is.na(api)) cli::cli_abort(c("x" = "{.val {x}} not recognized."))
  if (api != "care") return(api)

  case(grepl("/data-viewer?", x, perl = TRUE) ~ "current",
       grepl("/data?",        x, perl = TRUE) ~ "temporal")
}

#' @autoglobal
#' @noRd
dims_care <- function(x) {

  x <- switch(
    url_type(x$identifier),
    current = paste0(x$identifier, "?offset=0&size=1") |>
      request() |>
      perform_simple() |>
      get_elem("meta") |>
      get_elem(c("total_rows", "headers")),
    temporal = list(
      headers = paste0(x$identifier, "?offset=0&size=1") |>
        request() |>
        perform_simple() |>
        names(),
      total_rows = paste0(x$identifier, "/stats?offset=0&size=1") |>
        request() |>
        perform_simple() |>
        get_elem("total_rows")
    )
  )

  list(
    dims = class_dimensions(limit = api_limit("care"), rows = x$total_rows %||% 0L),
    fields = class_fields(x$headers)
  )
}

#' @autoglobal
#' @noRd
get_dims <- function(x) {

  type_url <- url_type(x$identifier)

  if (type_url %in% c("current", "temporal")) return(dims_care(x))

  id <- x$identifier |>
    append_url() |>
    request() |>
    req_error(is_error = ~ FALSE) |>
    perform_simple()

  list(
    dims = class_dimensions(limit = api_limit(type_url), rows = id$count %||% 0L),
    fields = class_fields(id$query$properties)
  )
}
