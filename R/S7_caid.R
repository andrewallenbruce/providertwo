#' @include S7_classes.R
NULL

#' Medicaid API Endpoint Classes
#' @name medicaid
#' @param alias `<chr>` endpoint alias
#' @returns An S7 `<caid_endpoint>` or `<caid_temporal>` object
#' @examples
#' caid_endpoint("managed_longterm")
#' caid_temporal("state_drug_util")
NULL

#' @rdname medicaid
#' @autoglobal
#' @export
caid_endpoint <- new_class(
  name        = "caid_endpoint",
  parent      = class_endpoint,
  package     = NULL,
  constructor = function(alias) {

    x <- select_caid(alias)

    new_object(
      class_endpoint(),
      identifier  = x$identifier,
      metadata    = get_metadata(x),
      dimensions  = get_dimensions(x)
    )
  }
)

#' @rdname medicaid
#' @autoglobal
#' @export
caid_temporal <- new_class(
  name        = "caid_temporal",
  parent      = class_temporal,
  package     = NULL,
  constructor = function(alias) {

    x <- select_caid_temp(alias)

    new_object(
      class_temporal(),
      metadata    = get_metadata(x),
      dimensions  = get_dimensions(x),
      endpoints   = x$endpoints
    )
  }
)
