#' @include S7_classes.R
NULL

#' Medicare API Endpoint Classes
#' @name medicare
#' @param alias `<chr>` endpoint alias
#' @param call `<env>` environment to use for error reporting
#' @param ... Additional arguments passed to the group constructor
#' @returns An S7 `<care_endpoint>`, or `<care_temporal>` object
#' @examples
#' care_endpoint("care_dialysis")
#' care_temporal("quality_payment")
#' care_group("care_hospital")
NULL

#' @autoglobal
#' @rdname medicare
#' @export
care_endpoint <- new_class(
  name        = "care_endpoint",
  parent      = class_endpoint,
  package     = NULL,
  constructor = function(alias) {

    x <- select_care(alias)

    new_object(
      class_endpoint(),
      identifier  = x$identifier,
      metadata    = get_metadata(x),
      dimensions  = care_dimensions(x)
    )
  }
)

#' @autoglobal
#' @rdname medicare
#' @export
care_temporal <- new_class(
  name        = "care_temporal",
  parent      = class_temporal,
  package     = NULL,
  constructor = function(alias) {

    x <- select_care_temp(alias)

    new_object(
      class_temporal(),
      metadata    = get_metadata(x),
      dimensions  = care_dimensions(x),
      endpoints   = x$endpoints
    )
  }
)

