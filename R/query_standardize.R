#' @autoglobal
#' @noRd
query_standardize <- function(obj, qry) {

  fields <- tolower(names(obj@access@dimensions@fields))
  params <- names(qry@params)
  p_idx  <- which_(params %in_% fields)
  f_idx  <- which_(fields %in_% params)

  new_fields <- obj@access@dimensions@fields[f_idx]

  new_params <- unname(qry@params)[p_idx]

  set_names(new_params, names(new_fields))

}

# qry <- new_query(
#   first_name = starts_with("Andr"),
#   last_name  = contains("J"),
#   state_cd   = any_of(c("CA", "GA", "NY")),
#   city       = equals(c("Atlanta", "Los Angeles"), negate = TRUE),
#   npi        = npi_ex$k,
#   ccn        = "01256",
#   rate       = between(0.45, 0.67),
#   year       = 2014:2025)
#
# obj <- endpoint("enroll_prov")
#
# set_names(
#   qry@params[names(qry@params) %in% tolower(names(obj@access@dimensions@fields))],
#   names(obj@access@dimensions@fields)[tolower(names(obj@access@dimensions@fields)) %in% names(qry@params)]
#   )
