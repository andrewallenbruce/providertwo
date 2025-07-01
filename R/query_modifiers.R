#' Query Modifiers
#'
#' @name query-helpers
#' @param operator `<chr>`  operator to use; default is `"="`
#' @param value    `<any>`  value to compare against
#' @param x        `<chr>`  input
#' @param or_equal `<lgl>`  append "="; default is `FALSE`
#' @param negate   `<lgl>`  prepend "NOT"; default is `FALSE`
#' @returns        `<list>` of query parameters
#' @examplesIf rlang::is_interactive()
#' greater_than_(1)
#' starts_with_("foo")
#' in_(state.abb[10:15])
#' @noRd
NULL

#' @autoglobal
#' @rdname query-modifiers
#' @noRd
modifier_ <- function(operator, value) {

  check_required(value)

  if (is_missing(operator)) operator <- "="

  allowed <- c(
    "=",
    "!=",
    "<",
    "<=",
    ">",
    ">=",
    "IN",
    "NOT IN",
    "CONTAINS",
    "STARTS_WITH",
    "ENDS_WITH",
    "BETWEEN",
    "NOT BETWEEN"
  )

  arg_match0(operator, values = allowed)

  mod <- list(operator = operator,
              value = value)

  class(mod) <- "modifier"

  mod
}

#' @autoglobal
#' @rdname query-modifiers
#' @noRd
greater_than_ <- function(x, or_equal = FALSE) {

  check_number_decimal(x)
  check_bool(or_equal)

  modifier_(
    operator = ifelse(!or_equal, ">", ">="),
    value    = x)
}

#' @autoglobal
#' @rdname query-modifiers
#' @noRd
less_than_ <- function(x, or_equal = FALSE) {

  check_number_decimal(x)
  check_bool(or_equal)

  modifier_(
    operator = ifelse(!or_equal, "<", "<="),
    value    = x)
}

#' @autoglobal
#' @rdname query-modifiers
#' @noRd
starts_with_ <- function(x) {
  modifier_(
    operator = "STARTS_WITH",
    value    = x)
}

#' @autoglobal
#' @rdname query-modifiers
#' @noRd
ends_with_ <- function(x) {
  modifier_(
    operator = "ENDS_WITH",
    value    = x)
}

#' @autoglobal
#' @rdname query-modifiers
#' @noRd
contains_ <- function(x) {
  modifier_(
    operator = "CONTAINS",
    value    = x)
}

#' @autoglobal
#' @rdname query-modifiers
#' @noRd
between_ <- function(x, negate = FALSE) {

  check_number_decimal(x)
  check_bool(negate)

  modifier_(
    operator = ifelse(!negate, "BETWEEN", "NOT BETWEEN"),
    value    = x)
}

#' @autoglobal
#' @rdname query-modifiers
#' @noRd
in_ <- function(x, negate = FALSE) {

  check_bool(negate)

  modifier_(
    operator = ifelse(!negate, "IN", "NOT IN"),
    value    = x)
}
