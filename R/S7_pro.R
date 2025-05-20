#' @include S7_classes.R
NULL

#' Provider API Endpoint Class
#' @name provider
#' @param alias `<chr>` endpoint alias
#' @returns An S7 `<pro_endpoint>`
#' @examples
#' pro_endpoint("asc_facility")
#' @autoglobal
#' @export
pro_endpoint <- new_class(
  name        = "pro_endpoint",
  parent      = class_endpoint,
  package     = NULL,
  constructor = function(alias) {

    x <- select_pro(alias)

    new_object(
      class_endpoint(),
      identifier  = x$identifier,
      metadata    = get_metadata(x),
      dimensions  = get_dimensions(x)
    )
  }
)
