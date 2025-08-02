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
#' @name query_modifier
#' @returns An S7 `<class_modifier>` object.
NULL

#' @autoglobal
#' @noRd
is_modifier <- function(x) {
  S7::S7_inherits(x, class_modifier)
}

#' @autoglobal
#' @noRd
are_modifiers <- function(x) {
  purrr::map_lgl(x, is_modifier)
}

#' @autoglobal
#' @noRd
any_modifiers <- function(x) {
  any(are_modifiers(x), na.rm = TRUE)
}

#' @rdname query_modifier
#' @examples
#' any_of(state.abb[10:15])
#' @autoglobal
#' @export
any_of <- S7::new_class(
  name        = "any_of",
  package     = NULL,
  parent      = class_modifier,
  constructor = function(x) {

    S7::new_object(
      class_modifier(),
      operator = "IN",
      value    = x)
  }
)

#' @rdname query_modifier
#' @examples
#' none_of(state.abb[1:3])
#' @autoglobal
#' @export
none_of <- S7::new_class(
  name        = "none_of",
  package     = NULL,
  parent      = class_modifier,
  constructor = function(x) {

    S7::new_object(
      class_modifier(),
      operator = "NOT+IN",
      value    = x)
  }
)

#' @rdname query_modifier
#' @examples
#' all_but(10000.23)
#' @autoglobal
#' @export
all_but <- S7::new_class(
  name        = "all_but",
  package     = NULL,
  parent      = class_modifier,
  constructor = function(x) {

    S7::new_object(
      class_modifier(),
      operator = "<>",
      value    = x)
  }
)

#' @rdname query_modifier
#' @examples
#' between(1000, 1100)
#' between(0.125, 2)
#' try(between(0.95, 0.67))
#' @autoglobal
#' @export
between <- S7::new_class(
  name        = "between",
  package     = NULL,
  parent      = class_modifier,
  constructor = function(x, y) {

    check_number_decimal(x)
    check_number_decimal(y)

    if (x >= y) {
      cli::cli_abort(
        "{.field x} [{.val {x}}] must be less than {.field y} [{.val {y}}]",
        call. = FALSE)
    }

    S7::new_object(
      class_modifier(),
      operator = "BETWEEN",
      value    = c(x, y))
  }
)

#' @rdname query_modifier
#' @examples
#' not_between(1000, 1100)
#' not_between(0.125, 2)
#' try(not_between(0.95, 0.67))
#' @autoglobal
#' @export
not_between <- S7::new_class(
  name        = "not_between",
  package     = NULL,
  parent      = class_modifier,
  constructor = function(x, y) {

    check_number_decimal(x)
    check_number_decimal(y)

    if (x >= y) {
      cli::cli_abort(
        "{.field x} [{.val {x}}] must be less than {.field y} [{.val {y}}]",
        call. = FALSE)
    }

    S7::new_object(
      class_modifier(),
      operator = "NOT+BETWEEN",
      value    = c(x, y))
  }
)

#' @rdname query_modifier
#' @examples
#' greater_than(1000)
#' greater_than(0.125, or_equal = TRUE)
#' @autoglobal
#' @export
greater_than <- S7::new_class(
  name        = "greater_than",
  package     = NULL,
  parent      = class_modifier,
  constructor = function(x, or_equal = FALSE) {

    check_number_decimal(x)
    check_bool(or_equal)

    S7::new_object(
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
less_than <- S7::new_class(
  name        = "less_than",
  package     = NULL,
  parent      = class_modifier,
  constructor = function(x, or_equal = FALSE) {

    check_number_decimal(x)
    check_bool(or_equal)

    S7::new_object(
      class_modifier(),
      operator = ifelse(!or_equal, "<", "<="),
      value    = x)
  }
)

#' @rdname query_modifier
#' @examples
#' starts_with("foo")
#' @autoglobal
#' @export
starts_with <- S7::new_class(
  name        = "starts_with",
  package     = NULL,
  parent      = class_modifier,
  constructor = function(x) {

    S7::new_object(
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
ends_with <- S7::new_class(
  name        = "ends_with",
  package     = NULL,
  parent      = class_modifier,
  constructor = function(x) {

    S7::new_object(
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
contains <- S7::new_class(
  name        = "contains",
  package     = NULL,
  parent      = class_modifier,
  constructor = function(x) {

    S7::new_object(
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
like <- S7::new_class(
  name        = "like",
  package     = NULL,
  parent      = class_modifier,
  constructor = function(x) {

    S7::new_object(
      class_modifier(),
      operator = "like",
      value    = x)
  }
)

# @param negate `<lgl>` prepend `NOT` to operator
# @rdname query_modifier
# @examples
# equals(1000)
# equals(1000, negate = TRUE)
# @autoglobal
# @export
# equals <- S7::new_class(
#   name        = "equals",
#   package     = NULL,
#   parent      = class_modifier,
#   constructor = function(x, negate = FALSE) {
#
#     check_bool(negate)
#
#     S7::new_object(
#       class_modifier(),
#       operator = ifelse(!negate, "=", "<>"),
#       value    = x)
#   }
# )
