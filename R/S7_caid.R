#' @include S7_classes.R
NULL

#' @noRd
#' @autoglobal
caid_dimensions <- new_class(
  name        = "caid_dimensions",
  parent      = class_dimensions,
  package     = NULL,
  constructor = function(x) {

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
        limit   = 1L
      ) |>
      perform_simple()

    new_object(
      class_dimensions(),
      limit  = 8000L,
      rows   = x$count,
      fields = x$query$properties)
  }
)

#' @noRd
#' @autoglobal
caid_metadata <- new_class(
  name        = "caid_metadata",
  parent      = class_metadata,
  package     = NULL,
  constructor = function(x) {
      new_object(
        class_metadata(),
        description = x$description,
        modified    = x$modified,
        dictionary  = x$dictionary,
        download    = x$download
      )
  }
)

#' Medicaid Endpoint
#' @param alias `<chr>` endpoint alias
#' @returns An S7 `<caid_endpoint>` object.
#' @examples
#' caid_endpoint("MLR")
#' caid_endpoint("enterprise")
#' @autoglobal
#' @rdname caid
#' @export
caid_endpoint <- new_class(
  name        = "caid_endpoint",
  parent      = class_endpoint,
  package     = NULL,
  constructor = function(alias) {

    x <- select_caid(alias)

    new_object(
      class_endpoint(),
      title       = x$title,
      identifier  = x$identifier,
      metadata    = caid_metadata(x),
      dimensions  = caid_dimensions(x)
    )
  }
)

#' Medicaid Endpoint Group
#' @param alias `<chr>` title alias
#' @returns An S7 `<caid_group>` object.
#' @examples
#' caid_group("demographics")
#' @autoglobal
#' @rdname caid
#' @export
caid_group <- new_class(
  name       = "caid_group",
  parent     = class_group,
  package    = NULL,
  properties = list(
    members  = new_property(
      class_list,
      getter = function(self)
        map(self@members, caid_endpoint) |>
        set_names(self@members)
      )
    ),
  constructor = function(alias) {

    x <- select_caid_group(alias)

    new_object(
      class_group(),
      group   = x$group,
      members = x$alias
    )
  }
)
