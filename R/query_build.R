#' @autoglobal
#' @noRd
params_care <- function(x) {
  set_names(query_care(x), names(x))
}
#' @autoglobal
#' @noRd
params_default <- function(x) {
  set_names(query_default(x), names(x))
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
#'    first_name            = starts_with("Andr"),
#'    middle_name           = ends_with("e"),
#'    last_name             = contains("J"),
#'    state_cd              = any_of(c("CA", "GA", "NY")),
#'    state                 = none_of(c("GA", "NY")),
#'    city                  = all_but(c("Atlanta", "Los Angeles")),
#'    npi                   = npi_ex$k,
#'    covered_recipient_npi = npi_ex$k[1:5],
#'    ccn                   = "105101256",
#'    rate                  = between(0.45, 0.67),
#'    year                  = 2021:2025)
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
    params     = NULL,
    identifier = S7::prop(obj, "identifier"))
}

S7::method(build, list(class_temporal, S7::class_missing)) <- function(obj, qry) {

  id <- S7::prop(obj, "identifier")

  list(
    params     = NULL,
    identifier = rlang::set_names(
      collapse::get_elem(id, "identifier"),
      collapse::get_elem(id, "year")
    )
  )
}

S7::method(build, list(class_catalog, class_query)) <- function(obj, qry) {
  S7::prop(obj, "access") |> build(qry = qry)
}

S7::method(build, list(class_current, class_query)) <- function(obj, qry) {

  params <- query_match(obj, qry)

  list(
    params     = params %|||% params_default(params),
    identifier = S7::prop(obj, "identifier")
  )
}

S7::method(build, list(care_current, class_query)) <- function(obj, qry) {

  params <- query_match(obj, qry)

  list(
    params     = params %|||% params_care(params),
    identifier = S7::prop(obj, "identifier")
  )
}

S7::method(build, list(class_temporal, class_query)) <- function(obj, qry) {

  id <- if ("year" %in% names(S7::prop(qry, "params"))) {
    collapse::sbt(S7::prop(obj, "identifier"), year %in% S7::prop(qry, "params")$year)
  } else {
      S7::prop(obj, "identifier")
  }

  params <- query_match(obj, qry)

  list(
    params     = params %|||% params_default(params),
    identifier = rlang::set_names(
      collapse::get_elem(id, "identifier"),
      collapse::get_elem(id, "year")
    )
  )
}

S7::method(build, list(care_temporal, class_query)) <- function(obj, qry) {

  id <- if ("year" %in% names(S7::prop(qry, "params"))) {
    collapse::sbt(S7::prop(obj, "identifier"), year %in% S7::prop(qry, "params")$year)
  } else {
    S7::prop(obj, "identifier")
  }

  params <- query_match(obj, qry)

  list(
    params     = params %|||% params_care(params),
    identifier = rlang::set_names(
      collapse::get_elem(id, "identifier"),
      collapse::get_elem(id, "year")
    )
  )
}
