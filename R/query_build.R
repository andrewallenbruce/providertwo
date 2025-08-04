#' @autoglobal
#' @noRd
params_fmt <- function(x = NULL, is_care = FALSE) {
  if (is.null(x)) return(NULL)
  set_names(`if`(is_care, query_care(x), query_default(x)), names(x))
}

#' @autoglobal
#' @noRd
params_flatten <- function(identifier, params = NULL) {
  if (is.null(params)) return(identifier)
  paste0(identifier, "&", paste0(unlist(params, use.names = FALSE), collapse = "&"))
}

#' @autoglobal
#' @noRd
params_years <- function(obj, qry) {
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
#'   new_query(
#'     year = 2022:2024,
#'     state = any_of(c("CA", "GA", "NY"))))
#'
#' build(
#'   endpoint("enroll_prov"),
#'   new_query(
#'     first_name = starts_with("And"),
#'     last_name = ends_with("ce")))
#'
#' build(
#'   endpoint("dial_facility"),
#'   new_query(city = any_of("BIRMINGHAM")))
#'
#' build(
#'   endpoint("quality_payment"),
#'   new_query(
#'     year = 2017:2025,
#'     practice_state_or_us_territory = any_of(c("GA", "FL")),
#'     allowed_charges = less_than(10000)))
#'
#' build(
#'   endpoint("pdc_clinician"),
#'   new_query(
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

  pr <- query_match(obj, qry)
  pr <- params_fmt(pr)
  id <- params_flatten(obj@identifier, pr)

  x  <- `if`(
    is.null(pr),
    0L,
    perform_simple(request(id)) |> _[["count"]]
    )

  class_results(
    title  = obj@metadata$title,
    params = names(pr),
    base   = id,
    total  = obj@dimensions@rows,
    found  = x,
    limit  = obj@dimensions@limit
  )
}

S7::method(build, list(class_temporal, class_query)) <- function(obj, qry) {

  id <- params_years(obj, qry)
  pr <- query_match(obj, qry)
  pr <- params_fmt(pr)
  id <- map(id, function(x) params_flatten(x, pr))

  res <- map(id, request) |>
    req_perform_parallel(on_error = "continue") |>
    map(function(x) fparse(resp_body_string(x)) |>
          _[["count"]])

  class_results(
    title  = obj@metadata$title,
    params = names(pr),
    base   = id,
    total  = obj@dimensions@rows,
    found  = unlist(res, use.names = FALSE),
    limit  = obj@dimensions@limit
  )
}

S7::method(build, list(care_current, class_query)) <- function(obj, qry) {

  pr <- query_match(obj, qry)
  pr <- params_fmt(pr, is_care = TRUE)
  id <- params_flatten(obj@identifier, pr)

  x  <- `if`(
    is.null(pr),
    0L,
    get_elem(
      perform_simple(
        req_url_path_append(
          request(id),
          "stats")
        ),
      "found_rows")
    )

  class_results(
    title  = obj@metadata$title,
    params = names(pr),
    base   = id,
    total  = obj@dimensions@rows,
    found  = x,
    limit  = obj@dimensions@limit
  )
}

S7::method(build, list(care_temporal, class_query)) <- function(obj, qry) {

  id <- params_years(obj, qry)
  pr <- query_match(obj, qry)
  pr <- params_fmt(pr, is_care = TRUE)
  id <- map(id, function(x) params_flatten(x, pr))

  res <- map(id, function(x) req_url_path_append(request(x), "stats")) |>
    req_perform_parallel(on_error = "continue") |>
    map(function(x) fparse(resp_body_string(x)))

  class_results(
    title  = obj@metadata$title,
    params = names(pr),
    base   = id,
    total  = get_elem(res, "total_rows") |> unlist(use.names = FALSE),
    found  = get_elem(res, "found_rows") |> unlist(use.names = FALSE),
    limit  = 5000L
  )
}
