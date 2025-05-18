#' @include S7_classes.R
NULL

#' Open Payments API Endpoint Classes
#' @name openpayments
#' @param alias `<chr>` endpoint alias
#' @returns An S7 `<open_endpoint>`, `<open_group>`, `<open_temporal>`, or `<open_troup>` object
#' @examples
#' open_endpoint("profile_covered")
#' open_group("profile")
#' open_temporal("grouped_state_nature")
#' open_troup("payment_detailed")
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
open_group <- new_class(
  name        = "open_group",
  parent      = class_group,
  package     = NULL,
  constructor = function(alias) {

    x <- select_open_group(alias)

    new_object(
      class_group(),
      group   = x$group,
      members = set_names(map(x$alias, open_endpoint), x$alias)
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

#' @autoglobal
#' @rdname openpayments
#' @export
open_troup <- new_class(
  name        = "open_troup",
  parent      = class_group,
  package     = NULL,
  constructor = function(alias) {

    x <- select_open_troup(alias)

    new_object(
      class_group(),
      group   = x$group,
      members = set_names(map(x$alias, open_temporal), x$alias)
    )
  }
)

