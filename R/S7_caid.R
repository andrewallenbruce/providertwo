#' @include S7_classes.R
NULL

#' @noRd
#' @autoglobal
caid_dimensions <- function(x) {

    x <- x$identifier |>
      request() |>
      req_url_query(
        schema  = "false",
        keys    = "false",
        results = "false",
        count   = "true",
        format  = "json",
        rowIds  = "false",
        offset  = 0L,
        limit   = 1L) |>
      perform_simple()

    class_dimensions(
      limit  = 8000L,
      rows   = x$count,
      fields = x$query$properties
    )
}

#' Medicaid API Endpoint Classes
#' @name medicaid
#' @param alias `<chr>` endpoint alias
#' @returns An S7 `<caid_endpoint>`, `<caid_group>`, or `<caid_temporal>` object
#' @examples
#' caid_endpoint("managed_longterm")
#' caid_group("caid_dual_status")
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
      metadata    = class_metadata(x),
      dimensions  = caid_dimensions(x)
    )
  }
)

#' @rdname medicaid
#' @autoglobal
#' @export
caid_group <- new_class(
  name        = "caid_group",
  parent      = class_group,
  package     = NULL,
  constructor = function(alias) {

    x <- select_caid_group(alias)

    new_object(
      class_group(),
      group   = x$group,
      members = set_names(map(x$alias, caid_endpoint), x$alias)
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
      metadata    = class_metadata(x),
      dimensions  = caid_dimensions(x),
      endpoints   = x$endpoints
    )
  }
)
