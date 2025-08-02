#' Standardize a query object to match the fields of an endpoint
#'
#' @param obj A `<class_care/caid/prov/open/hgov>` object.
#'
#' @param qry A `<class_query>` object.
#'
#' @returns A named list of query parameters that match the fields of the endpoint.
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
#'    ccn                   = "01256",
#'    rate                  = between(0.45, 0.67),
#'    year                  = 2021:2025)
#'
#' standardise(endpoint("enroll_prov"), qry)
#'
#' standardise(endpoint("dial_facility"), qry)
#'
#' standardise(endpoint("quality_payment"), qry)
#'
#' standardise(endpoint("pdc_clinician"), qry)
#'
#' standardise(endpoint("grp_cover_nature"), qry)
#'
#' standardise(collection("hha"), qry)
#'
#' standardise(collection("hhc"), qry)
#'
#' standardise(collection("hhc"))
#'
#' @autoglobal
#' @export
standardise <- S7::new_generic("standardise", c("obj", "qry"), function(obj, qry) {
  S7::S7_dispatch()
})

S7::method(standardise, list(class_group, class_query)) <- function(obj, qry) {
  S7::prop(obj, "members") |> purrr::map(\(x) standardise(obj = x, qry = qry))
}

S7::method(standardise, list(class_collection, class_query)) <- function(obj, qry) {
  S7::prop(obj, "members") |> purrr::map(\(x) standardise(obj = x, qry = qry))
}

S7::method(standardise, list(class_group, S7::class_missing)) <- function(obj, qry) {
  S7::prop(obj, "members") |> purrr::map(\(x) standardise(obj = x))
}

S7::method(standardise, list(class_catalog, S7::class_missing)) <- function(obj, qry) {
  S7::prop(obj, "access") |> standardise()
}

S7::method(standardise, list(class_current, S7::class_missing)) <- function(obj, qry) {

  list(
    params     = NULL,
    identifier = S7::prop(obj, "identifier"))
}

S7::method(standardise, list(class_temporal, S7::class_missing)) <- function(obj, qry) {

  id <- S7::prop(obj, "identifier")

  list(
    params     = NULL,
    identifier = rlang::set_names(
      collapse::get_elem(id, "identifier"),
      collapse::get_elem(id, "year")
    )
  )
}

S7::method(standardise, list(class_catalog, class_query)) <- function(obj, qry) {
  S7::prop(obj, "access") |> standardise(qry = qry)
}

S7::method(standardise, list(class_current, class_query)) <- function(obj, qry) {

  params <- query_standardise(obj, qry)

  list(
    params     = params %|||% set_names(query_default(params), names(params)),
    identifier = S7::prop(obj, "identifier")
  )
}

S7::method(standardise, list(care_current, class_query)) <- function(obj, qry) {

  params <- query_standardise(obj, qry)

  list(
    params     = params %|||% set_names(query_care(params), names(params)),
    identifier = S7::prop(obj, "identifier")
  )
}

S7::method(standardise, list(class_temporal, class_query)) <- function(obj, qry) {

  id <- if ("year" %in% names(S7::prop(qry, "params"))) {
    collapse::sbt(
      S7::prop(obj, "identifier"),
      year %in% S7::prop(qry, "params")$year)
  } else {
      S7::prop(obj, "identifier")
  }

  params <- query_standardise(obj, qry)

  list(
    params     = params %|||% set_names(query_default(params), names(params)),
    identifier = rlang::set_names(
      collapse::get_elem(id, "identifier"),
      collapse::get_elem(id, "year")
    )
  )
}

S7::method(standardise, list(care_temporal, class_query)) <- function(obj, qry) {

  id <- if ("year" %in% names(S7::prop(qry, "params"))) {
    collapse::sbt(
      S7::prop(obj, "identifier"),
      year %in% S7::prop(qry, "params")$year)
  } else {
      S7::prop(obj, "identifier")
  }

  params <- query_standardise(obj, qry)

  list(
    params     = params %|||% set_names(query_care(params), names(params)),
    identifier = rlang::set_names(
      collapse::get_elem(id, "identifier"),
      collapse::get_elem(id, "year")
    )
  )
}
