#' Query Modifiers
#'
#' @description
#' Helpers for use in constructing conditions in queries.
#'
#' @details
#' Query modifiers are a small DSL for use in constructing query conditions,
#' in the [JSON-API](https://www.drupal.org/docs/core-modules-and-themes/core-modules/jsonapi-module/filtering) format.
#'
#' @param x input
#' @param ... input
#' @param or_equal `<lgl>` append `=` to `>` or `<`
#' @name query_modifier
#' @returns An S7 `<class_modifier>` or `<class_junction>` object.
#' @source [URL Living Standard](https://url.spec.whatwg.org/#concept-url-query)
#' @source [JSON-API: Query Parameters](https://jsonapi.org/format/#query-parameters)
#' @source [JSON-API: Query Parameters Details](https://jsonapi.org/format/#appendix-query-details)
NULL

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
    if (length(self@operator) != 1) {
      cli::cli_abort(c("x" = "{.field @operator} must be length 1"), call = NULL)
    }
  }
)

#' @noRd
#' @autoglobal
class_junction <- S7::new_class(
  name       = "class_junction",
  package    = NULL,
  properties = list(
    conjunction = S7::class_character,
    members     = S7::class_character)
)

#' @autoglobal
#' @noRd
is_modifier <- function(x) {
  S7::S7_inherits(x, class_modifier)
}

#' @autoglobal
#' @noRd
is_junction <- function(x) {
  S7::S7_inherits(x, class_junction)
}

#' @rdname query_modifier
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

#' @rdname query_modifier
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
#' between(0.125, 2, 0.5)
#' between(0.95, 0.67, 0.75)
#' @autoglobal
#' @export
between <- S7::new_class(
  name        = "between",
  package     = NULL,
  parent      = class_modifier,
  constructor = function(...) {

    # check_number_decimal(x)

    x <- collapse::frange(c(...), na.rm = TRUE)

    S7::new_object(
      class_modifier(),
      operator = "BETWEEN",
      value    = c(x[1], x[2]))
  }
)

#' @rdname query_modifier
#' @examples
#' not_between(1000, 1100, 125)
#' not_between(0.125, 2, 0.5)
#' not_between(0.95, 0.67, 0.75)
#' @autoglobal
#' @export
not_between <- S7::new_class(
  name        = "not_between",
  package     = NULL,
  parent      = class_modifier,
  constructor = function(...) {

    # check_number_decimal(x)

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
