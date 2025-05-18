#' @include S7_classes.R
NULL

#' @noRd
#' @autoglobal
hgov_dimensions <- function(x) {

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

#' Healthcare.Gov API Endpoint Classes
#' @name healthcaregov
#' @param alias `<chr>` endpoint alias
#' @returns An S7 `<hgov_endpoint>`, or `<hgov_temporal>` object
#' @examples
#' hgov_endpoint("ab_registration_completion")
#' hgov_temporal("medical_loss_ratio")
NULL

#' @rdname healthcaregov
#' @autoglobal
#' @export
hgov_endpoint <- new_class(
  name        = "hgov_endpoint",
  parent      = class_endpoint,
  package     = NULL,
  constructor = function(alias) {

    x <- select_hgov(alias)

    new_object(
      class_endpoint(),
      identifier  = x$identifier,
      metadata    = class_metadata(x),
      dimensions  = hgov_dimensions(x)
    )
  }
)

#' @rdname healthcaregov
#' @autoglobal
#' @export
hgov_temporal <- new_class(
  name        = "hgov_temporal",
  parent      = class_temporal,
  package     = NULL,
  constructor = function(alias) {

    x <- select_hgov_temp(alias)

    new_object(
      class_temporal(),
      metadata    = class_metadata(x),
      dimensions  = hgov_dimensions(x),
      endpoints   = x$endpoints
    )
  }
)
