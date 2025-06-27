#' Query Formatting Helpers
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
#' @rdname query-helpers
#' @noRd
query_modifier_ <- function(operator, value) {

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

  list(FIELD    = character(0L),
       OPERATOR = operator,
       VALUE    = as.character(value))
}

#' @autoglobal
#' @rdname query-helpers
#' @noRd
greater_than_ <- function(x, or_equal = FALSE) {

  check_number_decimal(x)
  check_bool(or_equal)

  query_modifier_(
    operator = ifelse(!or_equal, ">", ">="),
    value    = x)
}

#' @autoglobal
#' @rdname query-helpers
#' @noRd
less_than_ <- function(x, or_equal = FALSE) {

  check_number_decimal(x)
  check_bool(or_equal)

  query_modifier_(
    operator = ifelse(!or_equal, "<", "<="),
    value    = x)
}

#' @autoglobal
#' @rdname query-helpers
#' @noRd
starts_with_ <- function(x) {
  query_modifier_(
    operator = "STARTS_WITH",
    value    = x)
}

#' @autoglobal
#' @rdname query-helpers
#' @noRd
ends_with_ <- function(x) {
  query_modifier_(
    operator = "ENDS_WITH",
    value    = x)
}

#' @autoglobal
#' @rdname query-helpers
#' @noRd
contains_ <- function(x) {
  query_modifier_(
    operator = "CONTAINS",
    value    = x)
}

#' @autoglobal
#' @rdname query-helpers
#' @noRd
between_ <- function(x, negate = FALSE) {

  check_number_decimal(x)
  check_bool(negate)

  query_modifier_(
    operator = ifelse(!negate, "BETWEEN", "NOT BETWEEN"),
    value    = x)
}

#' @autoglobal
#' @rdname query-helpers
#' @noRd
in_ <- function(x, negate = FALSE) {

  check_number_decimal(x)
  check_bool(negate)

  query_modifier_(
    operator = ifelse(!negate, "IN", "NOT IN"),
    value    = x)
}
