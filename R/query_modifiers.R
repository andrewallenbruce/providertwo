#' Query Modifiers
#'
#' @description
#' Helpers for use in constructing conditions in queries.
#'
#' @details
#' Query modifiers are a small DSL for use in constructing query conditions,
#' in the [JSON-API](https://www.drupal.org/docs/core-modules-and-themes/core-modules/jsonapi-module/filtering) format.
#'
#' @param x,y input
#' @param or_equal `<lgl>` append `=` to `>` or `<`
#' @param negate `<lgl>` prepend `NOT` to operator
#' @name query_modifier
#' @returns An S7 `<class_modifier>` object.
NULL


#' @noRd
#' @autoglobal
class_modifier <- new_class(
  name       = "class_modifier",
  package    = NULL,
  properties = list(
    operator = new_property(class_character, default = "="),
    value    = class_character | class_numeric,
    allowed  = new_property(
      class_character,
      default = c("caid", "prov", "open", "hgov", "care")
    )
  )
)

#' @autoglobal
#' @noRd
is_modifier <- function(x) {
  S7::S7_inherits(x, class_modifier)
}

#' @autoglobal
#' @noRd
are_modifiers <- function(x) {
  map_lgl(x, is_modifier)
}

#' @autoglobal
#' @noRd
any_modifiers <- function(x) {
  any(are_modifiers(x), na.rm = TRUE)
}

#' @rdname query_modifier
#' @examples
#' equals(1000)
#' equals(1000, negate = TRUE)
#' @autoglobal
#' @export
equals <- new_class(
  name        = "equals",
  package     = NULL,
  parent      = class_modifier,
  constructor = function(x, negate = FALSE) {

    check_bool(negate)

    new_object(
      class_modifier(),
      operator = ifelse(!negate, "=", "<>"),
      value    = x)
  }
)

#' @rdname query_modifier
#' @examples
#' greater_than(1000)
#' greater_than(0.125, or_equal = TRUE)
#' @autoglobal
#' @export
greater_than <- new_class(
  name        = "greater_than",
  package     = NULL,
  parent      = class_modifier,
  constructor = function(x, or_equal = FALSE) {

    check_number_decimal(x)
    check_bool(or_equal)

    new_object(
      class_modifier(),
      operator = ifelse(!or_equal, ">", ">="),
      value    = x)
  }
)

#' @rdname query_modifier
#' @examples
#' less_than(1000)
#' less_than(0.125, or_equal = TRUE)
#' @autoglobal
#' @export
less_than <- new_class(
  name        = "less_than",
  package     = NULL,
  parent      = class_modifier,
  constructor = function(x, or_equal = FALSE) {

    check_number_decimal(x)
    check_bool(or_equal)

    new_object(
      class_modifier(),
      operator = ifelse(!or_equal, "<", "<="),
      value    = x)
  }
)

#' @rdname query_modifier
#' @examples
#' between(1000, 1100)
#' between(0.125, 2, negate = TRUE)
#' @autoglobal
#' @export
between <- new_class(
  name        = "between",
  package     = NULL,
  parent      = class_modifier,
  constructor = function(x, y, negate = FALSE) {

    check_number_decimal(x)
    check_number_decimal(y)
    check_bool(negate)

    if (x >= y) cli::cli_abort("`x` must be less than `y`", call. = FALSE)
    if (length(c(x, y)) > 2) cli::cli_abort("`x` and `y` both must be length 1", call. = FALSE)

    new_object(
      class_modifier(),
      operator = ifelse(!negate, "BETWEEN", "NOT+BETWEEN"),
      value    = c(x, y))
  }
)

#' @rdname query_modifier
#' @examples
#' starts_with("foo")
#' @autoglobal
#' @export
starts_with <- new_class(
  name        = "starts_with",
  package     = NULL,
  parent      = class_modifier,
  constructor = function(x) {

    new_object(
      class_modifier(),
      operator = "STARTS_WITH",
      value    = x)
  }
)

#' @rdname query_modifier
#' @examples
#' ends_with("bar")
#' @autoglobal
#' @export
ends_with <- new_class(
  name        = "ends_with",
  package     = NULL,
  parent      = class_modifier,
  constructor = function(x) {

    new_object(
      class_modifier(),
      operator = "ENDS_WITH",
      value    = x)
  }
)

#' @rdname query_modifier
#' @examples
#' contains("baz")
#' @autoglobal
#' @export
contains <- new_class(
  name        = "contains",
  package     = NULL,
  parent      = class_modifier,
  constructor = function(x) {

    new_object(
      class_modifier(),
      operator = "CONTAINS",
      value    = x)
  }
)

#' @rdname query_modifier
#' @examples
#' like("baz")
#' @autoglobal
#' @export
like <- new_class(
  name        = "like",
  package     = NULL,
  parent      = class_modifier,
  constructor = function(x) {

    new_object(
      class_modifier(),
      operator = "like",
      value    = x)
  }
)

#' @rdname query_modifier
#' @examples
#' any_of(state.abb[10:15])
#' any_of(state.abb[10:15], negate = TRUE)
#' @autoglobal
#' @export
any_of <- new_class(
  name        = "any_of",
  package     = NULL,
  parent      = class_modifier,
  constructor = function(x, negate = FALSE) {

    check_bool(negate)

    new_object(
      class_modifier(),
      operator = ifelse(!negate, "IN", "NOT+IN"),
      value    = x)
  }
)
