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
#'    ccn        = "01256",
#'    rate       = between(0.45, 0.67),
#'    year       = 2014:2025)
#'
#' obj <- endpoint("enroll_prov")
#' standardise_query(obj, qry)
#'
#' obj <- endpoint("dial_facility")
#' standardise_query(obj, qry)
#'
#' obj <- endpoint("quality_payment")
#' standardise_query(obj, qry)
#' @autoglobal
#' @export
standardise_query <- function(obj, qry) {

  field_nm <- gsub(" ", "_", tolower(names(obj@access@dimensions@fields)), perl = TRUE)
  param_nm <- names(qry@params)

  set_names(
    qry@params[fmatch(field_nm, param_nm, nomatch = 0L)],
    names(obj@access@dimensions@fields[sort(fmatch(param_nm, field_nm, nomatch = 0L))])
    )
}
