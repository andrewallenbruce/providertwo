#' Query Modifiers
#'
#' @description
#' Helpers for use in constructing conditions in queries.
#'
#' @details
#' Query modifiers are part of a small DSL for use in query construction, in the [JSON-API](https://www.drupal.org/docs/core-modules-and-themes/core-modules/jsonapi-module/filtering) format.
#'
#' @param x input
#' @param ... input
#' @param or_equal `<lgl>` append `=` to `>` or `<`
#' @name query_modifier
#' @returns An S7 `<class_modifier>` object.
#' @source [JSON-API: Query Parameters](https://jsonapi.org/format/#query-parameters)
NULL

# query_modifier
#' @noRd
#' @autoglobal
class_modifier <- S7::new_class(
  name        = "class_modifier",
  package     = NULL,
  properties  = list(
    operator  = S7::new_property(S7::class_character, default = "="),
    value     = S7::class_character | S7::class_numeric,
    member_of = S7::class_character),
  validator   = function(self) {
    if (length(self@operator) != 1L) {
      cli::cli_abort(c("x" = "{.field @operator} must be length 1"), call = NULL)
    }
  }
)

#' @autoglobal
#' @noRd
is_modifier <- function(x) {
  S7::S7_inherits(x, class_modifier)
}

#' @rdname query_modifier
#' @examples
#' equal(1000)
#' @autoglobal
#' @export
equal <- S7::new_class(
  name        = "equal",
  package     = NULL,
  parent      = class_modifier,
  constructor = function(x) {

    S7::new_object(
      class_modifier(),
      operator = "=",
      value    = x)
  }
)

#' @rdname query_modifier
#' @examples
#' any_of(state.abb[10:15])
#' @autoglobal
#' @export
any_of <- S7::new_class(
  name        = "any_of",
  package     = NULL,
  parent      = class_modifier,
  constructor = function(...) {

    S7::new_object(
      class_modifier(),
      operator = "IN",
      value    = c(...))
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
  constructor = function(...) {

    S7::new_object(
      class_modifier(),
      operator = "NOT+IN",
      value    = c(...))
  }
)

#' @rdname query_modifier
#' @examples
#' not_equal(10000.23)
#' @autoglobal
#' @export
not_equal <- S7::new_class(
  name        = "not_equal",
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
#' between(1000, 1100, 125)
#' @autoglobal
#' @export
between <- S7::new_class(
  name        = "between",
  package     = NULL,
  parent      = class_modifier,
  constructor = function(...) {

    x <- collapse::frange(c(...), na.rm = TRUE)

    S7::new_object(
      class_modifier(),
      operator = "BETWEEN",
      value    = c(x[1], x[2]))
  }
)

#' @rdname query_modifier
#' @examples
#' not_between(0.95, 0.67, 0.75)
#' @autoglobal
#' @export
not_between <- S7::new_class(
  name        = "not_between",
  package     = NULL,
  parent      = class_modifier,
  constructor = function(...) {

    x <- collapse::frange(c(...), na.rm = TRUE)

    S7::new_object(
      class_modifier(),
      operator = "NOT+BETWEEN",
      value    = c(x[1], x[2]))
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

# @rdname query_modifier
#' @examplesIf interactive()
#' like("baz")
#' @autoglobal
#' @noRd
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
