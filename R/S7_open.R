#' @include S7_classes.R
NULL

#' Open Payments API Endpoint Classes
#' @name openpayments
#' @param alias `<chr>` endpoint alias
#' @returns An S7 `<open_endpoint>` or `<open_temporal>` object
#' @examples
#' open_endpoint("profile_covered")
#' open_temporal("grouped_state_nature")
NULL

#' @autoglobal
#' @rdname openpayments
#' @export
open_endpoint <- new_class(
  name        = "open_endpoint",
  parent      = class_endpoint,
  package     = NULL,
  constructor = function(alias) {

    x <- select_open(alias)

    new_object(
      class_endpoint(),
      identifier  = x$identifier,
      metadata    = get_metadata(x),
      dimensions  = get_dimensions(x)
    )
  }
)

#' @autoglobal
#' @rdname openpayments
#' @export
open_temporal <- new_class(
  name        = "open_temporal",
  parent      = class_temporal,
  package     = NULL,
  constructor = function(alias) {

    x <- select_open_temp(alias)

    new_object(
      class_temporal(),
      metadata    = get_metadata(x),
      dimensions  = get_dimensions(x),
      endpoints   = x$endpoints
    )
  }
)
