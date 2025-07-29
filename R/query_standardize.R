#' @autoglobal
#' @noRd
query_standardise <- function(obj, qry) {
  params <- prop(qry, "params")
  fields <- prop(obj, "dimensions") |> prop("fields")
  # If fields becomes a class -> prop(obj, "fields")

  # Use a modifier on "year" parameter if meant for the API?
  # `if`("year" %in% names(params), names(params)[names(params) != "year"], names(params))
  param_names <- names(params)[names(params) != "year"]
  field_clean <- gsub(" ", "_", tolower(fields), perl = TRUE)

  set_names(
    params[fmatch(field_clean, param_names, nomatch = 0L)],
    fields[sort(fmatch(param_names, field_clean, nomatch = 0L))])
}

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
#'    last_name             = contains("J"),
#'    state_cd              = any_of(c("CA", "GA", "NY")),
#'    state                 = any_of(c("GA", "NY")),
#'    city                  = equals(c("Atlanta", "Los Angeles"), negate = TRUE),
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
#' @autoglobal
#' @export
standardise <- new_generic("standardise", c("obj", "qry"), function(obj, qry) {
  S7_dispatch()
})

method(standardise, list(class_group, class_query)) <- function(obj, qry) {
  prop(obj, "members") |>
    map(\(x) standardise(obj = x, qry = qry))
}

method(standardise, list(class_collection, class_query)) <- function(obj, qry) {
  prop(obj, "members") |>
    map(\(x) standardise(obj = x, qry = qry))
}

method(standardise, list(class_catalog, class_missing)) <- function(obj, qry) {
  prop(obj, "access") |>
    standardise()
}

method(standardise, list(class_catalog, class_query)) <- function(obj, qry) {
  prop(obj, "access") |>
    standardise(qry = qry)
}

method(standardise, list(class_current, class_missing)) <- function(obj, qry) {
  list(query = NULL, identifier = prop(obj, "identifier"))
}

method(standardise, list(class_temporal, class_missing)) <- function(obj, qry) {
  id <- prop(obj, "identifier")

  list(query = NULL,
       identifier = set_names(get_elem(id, "identifier"), get_elem(id, "year")))
}

method(standardise, list(class_current, class_query)) <- function(obj, qry) {

  new_params <- query_standardise(obj, qry)

  list(
    query = set_names(query_default(new_params), names(new_params)),
    identifier = prop(obj, "identifier")
  )
}

method(standardise, list(care_current, class_query)) <- function(obj, qry) {
  new_params <- query_standardise(obj, qry)

  list(query      = set_names(query_care(new_params), names(new_params)),
       identifier = prop(obj, "identifier"))
}

method(standardise, list(class_temporal, class_query)) <- function(obj, qry) {
  id <- if ("year" %in% names(prop(qry, "params"))) {
    sbt(prop(obj, "identifier"), year %in% prop(qry, "params")$year)
  } else {
    prop(obj, "identifier")
  }

  new_params <- query_standardise(obj, qry)

  list(
    query      = set_names(query_default(new_params), names(new_params)),
    identifier = set_names(get_elem(id, "identifier"), get_elem(id, "year"))
  )
}

method(standardise, list(care_temporal, class_query)) <- function(obj, qry) {
  id <- if ("year" %in% names(prop(qry, "params"))) {
    sbt(prop(obj, "identifier"), year %in% prop(qry, "params")$year)
  } else {
    prop(obj, "identifier")
  }

  new_params <- query_standardise(obj, qry)

  list(
    query      = set_names(query_care(new_params), names(new_params)),
    identifier = set_names(get_elem(id, "identifier"), get_elem(id, "year"))
  )
}
