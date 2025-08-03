#' @autoglobal
#' @noRd
params_care <- function(x) {
  if (is.null(x)) return(NULL)
  set_names(query_care(x), names(x))
}

#' @autoglobal
#' @noRd
params_default <- function(x) {
  if (is.null(x)) return(NULL)
  set_names(query_default(x), names(x))
}

#' @autoglobal
#' @noRd
params_flatten <- function(id, x = NULL) {
  if (is.null(x)) return(id)
  paste0(id, "&", paste0(unlist(x, use.names = FALSE), collapse = "&"))
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
#' qry <- new_query(
#'    first_name = starts_with("An"),
#'    middle_name = ends_with("e"),
#'    last_name = contains("J"),
#'    state = any_of(c("CA", "GA", "NY")),
#'    practice_state_or_us_territory = any_of(c("CA", "GA", "NY")),
#'    year = 2021:2025)
#'
#' build(endpoint("enroll_prov"), qry)
#'
#' build(endpoint("dial_facility"), qry)
#'
#' build(endpoint("quality_payment"), qry)
#'
#' build(endpoint("pdc_clinician"), qry)
#'
#' build(endpoint("grp_cover_nature"), qry)
#'
#' build(collection("hha"), qry)
#'
#' build(collection("hhc"), qry)
#'
#' @autoglobal
#' @export
build <- S7::new_generic("build", c("obj", "qry"), function(obj, qry) {
  S7::S7_dispatch()
})

S7::method(build, list(class_group, class_query)) <- function(obj, qry) {
  S7::prop(obj, "members") |> purrr::map(\(x) build(obj = x, qry = qry))
}

S7::method(build, list(class_collection, class_query)) <- function(obj, qry) {
  S7::prop(obj, "members") |> purrr::map(\(x) build(obj = x, qry = qry))
}

S7::method(build, list(class_group, S7::class_missing)) <- function(obj, qry) {
  S7::prop(obj, "members") |> purrr::map(\(x) build(obj = x))
}

S7::method(build, list(class_catalog, S7::class_missing)) <- function(obj, qry) {
  S7::prop(obj, "access") |> build()
}

S7::method(build, list(class_current, S7::class_missing)) <- function(obj, qry) {
  list(
    title      = S7::prop(obj, "metadata")$title,
    dimensions = S7::prop(obj, "dimensions"),
    identifier = S7::prop(obj, "identifier"),
    params     = NULL
  )
}

S7::method(build, list(class_temporal, S7::class_missing)) <- function(obj, qry) {

  id <- S7::prop(obj, "identifier")

  list(
    title      = S7::prop(obj, "metadata")$title,
    dimensions = S7::prop(obj, "dimensions"),
    identifier = rlang::set_names(collapse::get_elem(id, "identifier"), collapse::get_elem(id, "year")),
    params     = NULL
  )
}

S7::method(build, list(class_catalog, class_query)) <- function(obj, qry) {
  S7::prop(obj, "access") |> build(qry = qry)
}

S7::method(build, list(class_current, class_query)) <- function(obj, qry) {

  params <- query_match(obj, qry)

  list(
    title      = S7::prop(obj, "metadata")$title,
    dimensions = S7::prop(obj, "dimensions"),
    identifier = S7::prop(obj, "identifier"),
    params     = params_default(params)
  )
}

S7::method(build, list(class_temporal, class_query)) <- function(obj, qry) {

  if ("year" %!in_% names(S7::prop(qry, "params"))) {

    id <- S7::prop(obj, "identifier") |>
      collapse::get_elem("^year$|^identifier$", regex = TRUE)

  } else {

    id <- S7::prop(obj, "identifier") |>
      collapse::sbt(year %in_% S7::prop(qry, "params")$year) |>
      collapse::get_elem("^year$|^identifier$", regex = TRUE)
  }

  params <- query_match(obj, qry)

  list(
    title      = S7::prop(obj, "metadata")$title,
    dimensions = S7::prop(obj, "dimensions"),
    identifier = set_names(id$identifier, id$year),
    params     = params_default(params)
  )
}

S7::method(build, list(care_current, class_query)) <- function(obj, qry) {

  params <- query_match(obj, qry)

  flist(
    title      = S7::prop(obj, "metadata")$title,
    params     = params_care(params),
    identifier = params_flatten(id = S7::prop(obj, "identifier"), x = params),
    dimensions = S7::prop(obj, "dimensions"),
    results    = request(identifier) |>
      req_url_path_append("stats") |>
      perform_simple() |>
      _[["data"]] |>
      _[["found_rows"]]
  )
}

S7::method(build, list(care_temporal, class_query)) <- function(obj, qry) {

  if ("year" %!in_% names(S7::prop(qry, "params"))) {

    id <- S7::prop(obj, "identifier") |>
      collapse::get_elem("^year$|^identifier$", regex = TRUE)

  } else {

    id <- S7::prop(obj, "identifier") |>
      collapse::sbt(year %in_% S7::prop(qry, "params")$year) |>
      collapse::get_elem("^year$|^identifier$", regex = TRUE)
  }

  params <- query_match(obj, qry)

  flist(
    title      = S7::prop(obj, "metadata")$title,
    params     = params_care(params),
    identifier = map(id$identifier, function(x) params_flatten(id = x, x = params)) |> set_names(id$year),
    dimensions = S7::prop(obj, "dimensions"),
    results    = map(identifier, function(x)
      request(x) |> req_url_path_append("stats")) |>
      req_perform_parallel(on_error = "continue") |>
      map(function(x)
        fparse(resp_body_string(x))) |>
      set_names(id$year)
  )
}
