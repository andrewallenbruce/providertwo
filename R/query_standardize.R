# If fields becomes a class -> prop(obj, "fields")
# Use a modifier on "year" parameter if meant for the API?
#' @autoglobal
#' @noRd
query_standardise <- function(obj, qry) {
  params <- S7::prop(qry, "params")
  fields <- S7::prop(obj, "dimensions") |> S7::prop("fields")

  # Remove "year"
  param_names <- names(params)[names(params) != "year"]

  # Convert to lowercase
  field_clean <- tolower(fields)
  # Replace space with underscore
  field_clean <- gsub(" ", "_", field_clean, perl = TRUE)
  # Remove dash followed by underscore
  field_clean <- gsub("-_", "", field_clean, perl = TRUE)

  x <- rlang::set_names(
    params[collapse::fmatch(field_clean, param_names, nomatch = 0L)],
    fields[sort(collapse::fmatch(param_names, field_clean, nomatch = 0L))])

  if (rlang::is_empty(x)) {
    cli::cli_alert_warning("No fields matched in {.field {S7::prop(obj, 'metadata')$title}}.")
    return(NULL)
  }
  x
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
