#' @include S7_classes.R
NULL

#' Provider API Endpoint Classes
#' @name provider
#' @param alias `<chr>` endpoint alias
#' @returns An S7 `<pro_endpoint>` or`<pro_group>` object
#' @examples
#' pro_endpoint("asc_facility")
#' pro_group("pro_dialysis")
NULL

#' @autoglobal
#' @rdname provider
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

#' @autoglobal
#' @rdname provider
#' @export
pro_group <- new_class(
  name       = "pro_group",
  parent      = class_group,
  package    = NULL,
  constructor = function(alias) {

    x <- select_pro_group(alias)

    new_object(
      class_group(),
      group   = x$group,
      members = set_names(map(x$alias, pro_endpoint), x$alias)
    )
  }
)
