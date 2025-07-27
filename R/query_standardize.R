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
# standardise_query(obj, qry)
#
#' @autoglobal
#' @noRd
standardise_query <- function(obj, qry) {

  fields <- tolower(names(obj@access@dimensions@fields))
  params <- names(qry@params)

  set_names(
    qry@params[
      collapse::fmatch(fields, params, nomatch = 0L)
      ],
    names(obj@access@dimensions@fields[
      sort(collapse::fmatch(params, fields, nomatch = 0L))
      ]
    )
  )
}
