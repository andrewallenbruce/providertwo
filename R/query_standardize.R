#' Standardise a query object to match the fields of an endpoint
#'
#' @param obj A `<class_care/caid/prov/open/hgov>` object.
#'
#' @param qry A `<class_query>` object.
#'
#' @returns A named list of query parameters that match the fields of the endpoint.
#'
#' @examples
#' qry <- new_query(
#'    first_name = starts_with("Andr"),
#'    last_name  = contains("J"),
#'    state_cd   = any_of(c("CA", "GA", "NY")),
#'    city       = equals(c("Atlanta", "Los Angeles"), negate = TRUE),
#'    npi        = npi_ex$k,
#'    covered_recipient_npi = npi_ex$k,
#'    ccn        = "01256",
#'    rate       = between(0.45, 0.67),
#'    year       = 2014:2025)
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
#' @autoglobal
#' @export
standardise <- new_generic("standardise", c("obj", "qry"), function(obj, qry) {
  S7_dispatch()
})

method(standardise, list(class_catalog, class_query)) <- function(obj, qry) {
  prop(obj, "access") |>
    standardise(qry = qry)
}

method(standardise, list(class_current, class_query)) <- function(obj, qry) {

  field_nm <- gsub(" ", "_", tolower(names(obj@dimensions@fields)), perl = TRUE)
  param_nm <- names(prop(qry, "params"))

  new_params <- set_names(
    qry@params[fmatch(field_nm, param_nm, nomatch = 0L)],
    names(obj@dimensions@fields[sort(fmatch(param_nm, field_nm, nomatch = 0L))])
  )

  list(
    params = new_params,
    query = set_names(query_default(new_params), names(new_params)),
    identifier = obj@identifier
  )
}

method(standardise, list(care_current, class_query)) <- function(obj, qry) {

  field_nm <- gsub(" ", "_", tolower(names(obj@dimensions@fields)), perl = TRUE)
  param_nm <- names(prop(qry, "params"))

  new_params <- set_names(
    qry@params[fmatch(field_nm, param_nm, nomatch = 0L)],
    names(obj@dimensions@fields[sort(fmatch(param_nm, field_nm, nomatch = 0L))])
  )

  list(
    params = new_params,
    query = set_names(query_care(new_params), names(new_params)),
    identifier = obj@identifier
  )
}

method(standardise, list(class_temporal, class_query)) <- function(obj, qry) {

  field_nm <- gsub(" ", "_", tolower(names(obj@dimensions@fields)), perl = TRUE)
  param_nm <- names(prop(qry, "params"))

  id <- prop(obj, "identifier")

  if ("year" %in% param_nm) {
    id <- sbt(id, year %in% qry@params$year)

    if (is_empty(id)) {
      cli::cli_warn(
        c("{.field year = {paste0(range(qry@params$year), collapse = ':')}} had {nrow(id)} matches."),
        call = call)
      invisible(NULL)
    }
  }

  new_params <- set_names(
    qry@params[fmatch(field_nm, param_nm, nomatch = 0L)],
    names(obj@dimensions@fields[sort(fmatch(param_nm, field_nm, nomatch = 0L))])
  )

  list(
    params = new_params,
    query = set_names(query_default(new_params), names(new_params)),
    identifier = set_names(get_elem(id, "identifier"), get_elem(id, "year"))
  )
}

method(standardise, list(care_temporal, class_query)) <- function(obj, qry) {

  field_nm <- gsub(" ", "_", tolower(names(obj@dimensions@fields)), perl = TRUE)
  param_nm <- names(prop(qry, "params"))

  id <- prop(obj, "identifier")

  if ("year" %in% param_nm) {
    id <- sbt(id, year %in% qry@params$year)

    if (is_empty(id)) {
      cli::cli_warn(
        c("{.field year = {paste0(range(qry@params$year), collapse = ':')}} had {nrow(id)} matches."),
        call = call)
      invisible(NULL)
    }
  }

  new_params <- set_names(
    qry@params[fmatch(field_nm, param_nm, nomatch = 0L)],
    names(obj@dimensions@fields[sort(fmatch(param_nm, field_nm, nomatch = 0L))])
  )

  list(
    params = new_params,
    query = set_names(query_care(new_params), names(new_params)),
    identifier = set_names(get_elem(id, "identifier"), get_elem(id, "year"))
  )
}









#' @autoglobal
#' @noRd
standardise_query <- function(obj, qry) {

  field_nm <- gsub(" ", "_", tolower(names(obj@access@dimensions@fields)), perl = TRUE)
  param_nm <- names(qry@params)

  set_names(
    qry@params[fmatch(field_nm, param_nm, nomatch = 0L)],
    names(obj@access@dimensions@fields[sort(fmatch(param_nm, field_nm, nomatch = 0L))])
  )
}
