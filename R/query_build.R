#' @autoglobal
#' @noRd
cli_no_match <- function(obj) {
  cli::cli_alert_warning(
    c(
      "No query {.field parameters} matched to endpoint {.field fields} \n",
      "{cli::symbol$record} {.field {title(obj)}}"
    )
  )
}

#' @autoglobal
#' @noRd
null_zero <- function(x, alt_expr) {
  `if`(is_null(x), 0L, alt_expr)
}

#' @autoglobal
#' @noRd
params_fmt <- function(x = NULL, is_care = FALSE) {
  if (is_null(x) || is_empty(x)) return(NULL)
  set_names(`if`(is_care, query_care(x), query_default(x)), names(x))
}

#' @autoglobal
#' @noRd
params_flatten <- function(url, params = NULL) {
  if (is_null(params) || is_empty(params)) return(url)
  paste0(url, "&", paste0(unlist(params, use.names = FALSE), collapse = "&"))
}

#' @autoglobal
#' @noRd
params_years <- function(obj, qry) {

  # if (is_empty(x)) cli::cli_abort(c("x" = "{.field year(s)} {.val {qry@params$year}} had {nrow(x)} matches."), call. = FALSE)

  x <- if ("year" %in_% names(qry@params)) {
    sbt(obj@identifier, year %in_% qry@params$year) |>
      get_elem("^year$|^identifier$", regex = TRUE)
  } else {
    obj@identifier |>
      get_elem("^year$|^identifier$", regex = TRUE)
  }
  set_names(x$identifier, x$year)
}

#' Build a Query for an Endpoint
#'
#' @param obj An `<endpoint>`, `<collection>` or `<group>` object.
#'
#' @param qry A `<query>` object.
#'
#' @returns A list of query parameters matched to an endpoint's fields.
#'
#' @examples
#' build(
#'   endpoint("drug_state"),
#'   query(
#'     year = 2022:2024,
#'     state = any_of(c("CA", "GA", "NY"))))
#'
#' build(
#'   endpoint("enroll_prov"),
#'   query(
#'     first_name = starts_with("And"),
#'     last_name = ends_with("ce")))
#'
#' build(
#'   endpoint("dial_facility"),
#'   query(city = any_of("BIRMINGHAM")))
#'
#' build(
#'   endpoint("quality_payment"),
#'   query(
#'     year = 2017:2025,
#'     practice_state_or_us_territory = any_of(c("GA", "FL")),
#'     allowed_charges = less_than(10000)))
#'
#' build(
#'   endpoint("pdc_clinician"),
#'   query(
#'     provider_first_name = starts_with("An"),
#'     provider_last_name = contains("J"),
#'     state = any_of(c("CA", "GA", "NY"))))
#'
#' @autoglobal
#' @export
build <- S7::new_generic("build", c("obj", "qry"), function(obj, qry) {
  S7::S7_dispatch()
})

S7::method(build, list(class_group, class_query)) <- function(obj, qry) {
  S7::prop(obj, "members") |> purrr::map(\(x) build(obj = x, qry = qry))
}

S7::method(build, list(class_catalog, class_query)) <- function(obj, qry) {
  S7::prop(obj, "access") |> build(qry = qry)
}

S7::method(build, list(class_current, class_query)) <- function(obj, qry) {
  prm <- query_match(obj, qry)

  if (is_empty(prm)) {
    cli_no_match(obj)
    return(
      class_results(
        title  = title(obj),
        base   = obj@identifier,
        total  = obj@dimensions@rows,
        found  = 0L,
        limit  = obj@dimensions@limit
      )
    )
  }

  prm <- params_fmt(prm)
  url <- append_url(obj@identifier) |> params_flatten(prm)
  res <- map(url, request) |>
    req_perform_parallel(on_error = "continue") |>
    map(function(x)
      parse_string(x, query = "count")) |>
    unlist(use.names = FALSE)

  class_results(
    title  = title(obj),
    params = names(prm),
    base   = url,
    total  = obj@dimensions@rows,
    found  = res %||% 0L,
    limit  = obj@dimensions@limit
  )
}

S7::method(build, list(class_temporal, class_query)) <- function(obj, qry) {

  id <- params_years(obj, qry)
  pr <- query_match(obj, qry)
  pr <- params_fmt(pr)
  id <- append_url(id) |> map(function(x) params_flatten(x, pr))

  res <- map(id, request) |>
    req_perform_parallel(on_error = "continue") |>
    map(function(x) parse_string(x, query = "count")) |>
    unlist(use.names = FALSE)

  class_results(
    title  = obj@metadata$title,
    params = names(pr),
    base   = id,
    total  = obj@dimensions@rows,
    found  = res,
    limit  = obj@dimensions@limit
  )
}

S7::method(build, list(care_current, class_query)) <- function(obj, qry) {
  prm <- query_match(obj, qry)

  if (is_empty(prm)) {
    cli_no_match(obj)
    return(
      class_results(
        title  = title(obj),
        base   = obj@identifier,
        total  = obj@dimensions@rows,
        found  = 0L,
        limit  = obj@dimensions@limit
      )
    )
  }

  prm <- params_fmt(prm, is_care = TRUE)
  url <- append_url(obj@identifier, "care") |> params_flatten(prm)
  res <- map(url, \(x) path_stats(request(x))) |>
    req_perform_parallel(on_error = "continue") |>
    map(function(x)
      parse_string(x, query = "found_rows")) |>
    unlist(use.names = FALSE)

  class_results(
    title  = title(obj),
    params = names(prm),
    base   = url,
    total  = obj@dimensions@rows,
    found  = res %||% 0L,
    limit  = obj@dimensions@limit
  )
}

S7::method(build, list(care_temporal, class_query)) <- function(obj, qry) {

  id <- params_years(obj, qry)
  pr <- query_match(obj, qry)
  pr <- params_fmt(pr, is_care = TRUE)
  id <- append_url(id, "care") |> map(function(x) params_flatten(x, pr))

  res <- map(id, function(x) path_stats(request(x))) |>
    req_perform_parallel(on_error = "continue") |>
    map(function(x) parse_string(x))

  class_results(
    title  = obj@metadata$title,
    params = names(pr),
    base   = id,
    total  = get_elem(res, "total_rows") |> unlist(use.names = FALSE),
    found  = get_elem(res, "found_rows") |> unlist(use.names = FALSE),
    limit  = obj@dimensions@limit
  )
}
