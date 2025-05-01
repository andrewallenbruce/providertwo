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

#' @noRd
#' @autoglobal
caid_metadata <- new_class(
  name          = "caid_metadata",
  parent        = class_metadata,
  package       = NULL,
  properties    = list(
    dictionary  = class_character,
    download    = class_character
    ),
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

#' Medicaid API Endpoint Classes
#' @name medicaid
#' @param alias `<chr>` endpoint alias
#' @returns An S7 `<caid_endpoint>`, `<caid_group>`, or `<caid_temporal>` object
#' @examples
#' caid_endpoint("enterprise")
#' caid_group("demographics")
#' caid_temporal("MCP")
NULL

#' @autoglobal
#' @rdname medicaid
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

#' @autoglobal
#' @rdname medicaid
#' @export
caid_group <- new_class(
  name        = "caid_group",
  parent      = class_group,
  package     = NULL,
  constructor = function(alias) {

    x <- select_caid_group(alias)

    new_object(
      class_group(),
      flass   = "caid_endpoint",
      group   = x$group,
      members = x$alias
    )
  }
)

#' @autoglobal
#' @rdname medicaid
#' @export
caid_temporal <- new_class(
  name        = "caid_temporal",
  parent      = class_temporal,
  package     = NULL,
  constructor = function(alias) {

    x <- select_caid_temp(alias)

    new_object(
      class_temporal(),
      title       = x$title,
      metadata    = class_metadata(x$description, x$modified),
      dimensions  = caid_dimensions(x),
      endpoints   = x$endpoints
    )
  }
)
