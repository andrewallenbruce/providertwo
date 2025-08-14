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
get_dims <- function(x) {

  if (x$catalog == "care") {
    return(
      switch(
        x$point,
        current = dims_care_current(x),
        temporal = dims_care_temporal(x)
      )
    )
  }


  i <- switch(
    x$point,
    current = paste0(x$identifier, "?count=true&results=true&offset=0&limit=1") |>
      request() |>
      req_error(is_error = ~ FALSE) |>
      perform_simple(),
    temporal = paste0(x$identifier, "?count=true&results=true&offset=0&limit=1") |>
      map(request) |>
      req_perform_parallel(on_error = "continue") |>
      resps_successes() |>
      map(function(x)
        parse_string(x)) |>
      set_names(x$year)
  )

  list(
    dims = class_dimensions(
      limit = api_limit(x$catalog),
      total = switch(
        x$point,
        current = i$count,
        temporal = unname(map_int(i, \(x) x[["count"]]))
      )
    ),
    fields = class_fields(get_elem(i, "properties"))
  )
}

#' @autoglobal
#' @noRd
dims_care_temporal <- function(x) {
  list(
    dims = class_dimensions(
      limit = api_limit(x$catalog),
      total = paste0(x$identifier, "/stats?offset=0&size=1") |>
        map(request) |>
        req_perform_parallel(on_error = "continue") |>
        resps_successes() |>
        map(function(x)
          parse_string(x) |>
            get_elem("total_rows")) |>
        set_names(x$year) |>
        unlist(use.names = FALSE)
    ),
    fields = class_fields(
      keys = paste0(x$identifier, "?offset=0&size=1") |>
        map(request) |>
        req_perform_parallel(on_error = "continue") |>
        resps_successes() |>
        map(function(x)
          names2(parse_string(x))) |>
        set_names(x$year)
    )
  )
}

#' @autoglobal
#' @noRd
dims_care_current <- function(x) {
  i <- paste0(x$identifier, "?offset=0&size=1") |>
    request() |>
    perform_simple() |>
    get_elem("meta") |>
    get_elem(c("total_rows", "headers"))

  list(
    dims = class_dimensions(
      limit = api_limit(x$catalog),
      total = i$total_rows),
    fields = class_fields(i$headers)
  )
}

#' @autoglobal
#' @noRd
reduce_fields <- function(x) {
  flist(
    f_union = purrr::reduce(x$headers, vctrs::vec_set_union) |>
      kit::psort(nThread = 4L),
    f_common = purrr::reduce(x$headers, vctrs::vec_set_intersect) |>
      kit::psort(nThread = 4L),
    f_unique = purrr::imap(x$headers, function(x, i)
      vctrs::vec_set_difference(x, y = f_common)) |>
      purrr::map(\(x) kit::psort(x, nThread = 4L))
  )
}
