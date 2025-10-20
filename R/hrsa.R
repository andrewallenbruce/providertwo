#' HRSA Facilities
#'
#' @param retrieve `<chr>` String indicating what to retrieve.
#'
#' @returns A list of endpoints.
#'
#' @examples
#' hrsa_facilities()
#'
#' hrsa_facilities("items")
#' @autoglobal
#' @export
hrsa_facilities <- function(retrieve = NULL) {

# https://data.hrsa.gov/tools/web-services/registration#serviceInfo

  hrsa_url  <- paste0("https://gisportal.hrsa.gov/server/rest/services/FeatureServices/CMSApprovedFacilities_FS/MapServer")
  hrsa_open <- arcgislayers::arc_open(hrsa_url)

  if (is.null(retrieve)) {
    return(hrsa_open)
  }
  switch(
    match.arg(retrieve, c("layers", "items")),
    layers = arcgislayers::get_all_layers(hrsa_open),
    # fields = arcgislayers::list_fields(hrsa_open),
    items  = arcgislayers::list_items(hrsa_open)
  )
}
