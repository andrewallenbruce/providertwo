#' Query Groups
#'
#' @description
#' Helpers for use in constructing groups in queries.
#'
#' @details
#' Query groupers are part of a small DSL for use in query construction, in the [JSON-API](https://www.drupal.org/docs/core-modules-and-themes/core-modules/jsonapi-module/filtering) format.
#'
#' @param ... input
#' @name query_grouper
#' @returns An S7 `<class_junction>` object.
#' @source [JSON-API: Query Parameters](https://jsonapi.org/format/#query-parameters)
NULL

# query_group
#' @noRd
#' @autoglobal
class_junction <- S7::new_class(
  name          = "class_junction",
  package       = NULL,
  properties    = list(
    conjunction = S7::class_character,
    members     = S7::class_character)
)

#' @autoglobal
#' @noRd
is_junction <- function(x) {
  S7::S7_inherits(x, class_junction)
}

#' @rdname query_grouper
#' @examples
#' and("foo", "bar")
#' @autoglobal
#' @export
and <- S7::new_class(
  name        = "and",
  package     = NULL,
  parent      = class_junction,
  constructor = function(...) {
    S7::new_object(
      class_junction(),
      conjunction = "AND",
      members     = c(...)
    )
  }
)

#' @rdname query_grouper
#' @examples
#' or("foo", "bar")
#' @autoglobal
#' @export
or <- S7::new_class(
  name        = "or",
  package     = NULL,
  parent      = class_junction,
  constructor = function(...) {
    S7::new_object(
      class_junction(),
      conjunction = "OR",
      members     = c(...)
    )
  }
)
