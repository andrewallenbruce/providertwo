#' @include S7_classes.R
NULL

#' @noRd
#' @autoglobal
open_dimensions <- function(x) {

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
    limit  = 500L,
    rows   = x$count,
    fields = x$query$properties
  )
}

#' @noRd
#' @autoglobal
open_metadata <- new_class(
  name       = "open_metadata",
  parent     = class_metadata,
  package    = NULL,
  properties = list(
    download = class_character
    ),
  constructor = function(x) {
    new_object(
      class_metadata(),
      description = x$description,
      modified    = x$modified,
      download    = x$download
    )
  }
)

#' Open Payments API Endpoint Classes
#' @name openpayments
#' @param alias `<chr>` endpoint alias
#' @returns An S7 `<open_endpoint>`, `<open_group>`, `<open_temporal>`, or `<open_troup>` object
#' @examples
#' open_endpoint("PROF_covered")
#' open_endpoint("SUMM_state_group")
#' open_group("profile")
#' open_group("summary")
#' open_temporal("DATA_general")
#' open_temporal("GROUP_all")
#' open_troup("grouped_payment")
#' open_troup("detailed_payment")
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
      title       = x$title,
      identifier  = x$identifier,
      metadata    = open_metadata(x),
      dimensions  = open_dimensions(x)
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
      flass   = "open_endpoint",
      group   = x$group,
      members = x$alias
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
      title       = x$title,
      metadata    = class_metadata(x$description, x$modified),
      dimensions  = open_dimensions(x),
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
      flass   = "open_temporal",
      group   = x$group,
      members = x$alias
    )
  }
)

