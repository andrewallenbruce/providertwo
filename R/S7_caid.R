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
caid_metadata <- function(x) {
  list(
    title       = x$title,
    description = x$description,
    modified    = x$modified,
    dictionary  = x$dictionary,
    download    = x$download
  )
}

#' Medicaid API Endpoint Classes
#' @name medicaid
#' @param alias `<chr>` endpoint alias
#' @returns An S7 `<caid_endpoint>`, `<caid_group>`, or `<caid_temporal>` object
#' @examples
#' caid_endpoint("MLR_summary")
#' caid_group("demographics")
#' caid_temporal("MCP")
NULL

#' @name caid_endpoint
#' @rdname medicaid
#' @autoglobal
#' @export
caid_endpoint := new_class(
  parent      = class_endpoint,
  package     = NULL,
  constructor = function(alias) {

    x <- select_caid(alias)

    new_object(
      class_endpoint(),
      identifier  = x$identifier,
      metadata    = caid_metadata(x),
      dimensions  = caid_dimensions(x)
    )
  }
)

#' @name caid_group
#' @rdname medicaid
#' @autoglobal
#' @export
caid_group := new_class(
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

#' @name caid_temporal
#' @rdname medicaid
#' @autoglobal
#' @export
caid_temporal := new_class(
  parent      = class_temporal,
  package     = NULL,
  constructor = function(alias) {

    x <- select_caid_temp(alias)

    new_object(
      class_temporal(),
      metadata    = list(title = x$title, description = x$description, modified = x$modified),
      dimensions  = caid_dimensions(x),
      endpoints   = x$endpoints
    )
  }
)
