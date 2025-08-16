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
  if (is.null(retrieve)) {
    return(the$clog$hrsa)
  }
  switch(
    match.arg(retrieve, c("layers", "items")),
    layers = arcgislayers::get_all_layers(the$clog$hrsa),
    # fields = arcgislayers::list_fields(the$clog$hrsa),
    items  = arcgislayers::list_items(the$clog$hrsa)
  )
}
