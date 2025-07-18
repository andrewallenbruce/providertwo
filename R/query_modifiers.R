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
#' @param or_equal `<lgl>` append `=`
#' @param negate `<lgl>` prepend `NOT`
#' @name query_modifier
#' @returns An object of class `<modifier>`
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
is_modifier_S7 <- function(x) {
  S7::S7_inherits(x, class_modifier)
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
    # check lengths of x and y?

    if (x >= y) cli::cli_abort("`x` must be less than `y`.", call. = FALSE)

    new_object(
      class_modifier(),
      operator = ifelse(!negate, "BETWEEN", "NOT+BETWEEN"),
      value    = c(x, y))
  }
)

#' @rdname query_modifier
#' @examples
#' starts_with_("foo")
#' starts_with_("foo", care = TRUE)
#' @autoglobal
#' @noRd
starts_with_ <- function(x, care = FALSE) {

  check_bool(care)

  modifier_(
    operator = if (!care) "starts+with" else "STARTS_WITH",
    value    = x,
    allow    = if (!care) c("caid", "prov", "open", "hgov") else "care")
}

#' @rdname query_modifier
#' @examples
#' ends_with_("bar")
#' @autoglobal
#' @noRd
ends_with_ <- function(x) {

  check_character(x, allow_na = FALSE)

  modifier_(
    operator = "ENDS_WITH",
    value    = x,
    allow    = "care")

}

#' @rdname query_modifier
#' @examples
#' contains_("baz")
#' contains_("baz", care = TRUE)
#' @autoglobal
#' @noRd
contains_ <- function(x, care = FALSE) {

  check_bool(care)

  modifier_(
    operator = if (!care) "contains" else "CONTAINS",
    value    = x,
    allow    = if (!care) c("caid", "prov", "open", "hgov") else "care")
}

#' @rdname query_modifier
#' @examples
#' like_("baz")
#' @autoglobal
#' @noRd
like_ <- function(x) {

  modifier_(
    operator = "like",
    value    = x,
    allow    = c("caid", "prov", "open", "hgov"))
}

#' @rdname query_modifier
#' @examples
#' in_(state.abb[10:15])
#' in_(state.abb[10:15], negate = TRUE)
#' in_(state.abb[1:5], care = TRUE)
#' in_(state.abb[1:5], care = TRUE, negate = TRUE)
#' @autoglobal
#' @noRd
in_ <- function(x, care = FALSE, negate = FALSE) {

  check_bool(negate)
  check_bool(care)

  modifier_(
    operator = if (!care) ifelse(!negate, "in", "not+in") else ifelse(!negate, "IN", "NOT+IN"),
    value    = x,
    allow    = if (!care) c("caid", "prov", "open", "hgov") else "care")
}

#' Query Modifier Constructor
#'
#' @param operator `<chr>` comparison operator
#' @param value `<any>` value to compare against
#' @param allow `<chr>` allowed endpoint class(es) for this modifier
#' @returns An object of class `"modifier"`
#' @examples
#' modifier_(">", 1000, "all")
#' @autoglobal
#' @keywords internal
#' @noRd
modifier_ <- function(operator, value, allow) {

  check_required(operator)
  check_required(allow)

  all  <- c("=", "<>", "<", "<=", ">", ">=")
  ohcp <- c("like", "between", "in", "not+in", "contains", "starts+with", "match")
  prov <- c("is_empty", "not_empty")
  care <- c("NOT+BETWEEN", "BETWEEN", "IN", "NOT+IN", "CONTAINS",
            "STARTS_WITH", "ENDS_WITH", "IS+NULL", "IS+NOT+NULL")

  arg_match0(operator, values = cheapr::cheapr_c(all, ohcp, prov, care))

  structure(
    list(
      operator = operator,
      value    = value,
      allow    = allow),
    class = "modifier")
}

#' Print method for query modifier
#' @param ... additional arguments
#' @rdname query_modifier
#' @method print modifier
#' @autoglobal
#' @noRd
print.modifier <- function(x, ...) {

  cli::cli_text(cli::col_cyan("<modifier>"))
  cli::cli_text(c(cli::col_silver("Operator: "), cli::col_red(x$operator)))

  if (!is.null(x$value)) {
    cli::cli_text(c(cli::col_silver("Value(s): "), cli::col_yellow("{x$value}")))
  }
  cli::cli_text(c(cli::col_silver("Allowed: "), brackets_cli2(sort(x$allow))))
}
