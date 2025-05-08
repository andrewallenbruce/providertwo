#' @include S7_classes.R
NULL

#' @noRd
#' @autoglobal
pro_dimensions <- function(x) {

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
    limit  = 2000L,
    rows   = x$count,
    fields = x$query$properties
  )
}

#' @noRd
#' @autoglobal
pro_metadata <- function(x) {
  list(
    title       = x$title,
    group       = x$group,
    description = x$description,
    issued      = x$issued,
    modified    = x$modified,
    released    = x$released,
    site        = x$site,
    dictionary  = x$dictionary,
    download    = x$download
  )
}

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
      metadata    = pro_metadata(x),
      dimensions  = pro_dimensions(x)
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
