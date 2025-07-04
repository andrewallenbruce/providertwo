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

# list(
#   first_name = starts_with_("Andr"),
#   middle_name = ends_with_("e"),
#   last_name = contains_("J"),
#   state = in_(c("CA", "GA", "NY")),
#   country = not_in_("USA"),
#   state_owner = c("GA", "MD"),
#   npi = npi_ex$k,
#   ccn = "01256",
#   zip = is_("30303"),
#   rate = greater_or_equal_(1000),
#   rate2 = less_than_(1000),
#   rate2 = less_or_equal_(1000),
#   rate3 = greater_than_(1000),
#   rate3 = not_between_(2000),
#   rate4 = between_(2000),
#   rate5 = not_in_(c(1000, 2000)),
#   rate6 = is_not_(1000),
#   rate7 = blank_(""),
#   rate8 = not_blank_("")
# )

#' @autoglobal
#' @rdname query-modifiers
#' @noRd
modifier_ <- function(operator, value) {

  check_required(value)

  if (is_missing(operator)) operator <- "="

  allowed <- c(
    "=",
    "<>",
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
    "NOT BETWEEN",
    "IS NULL",
    "IS NOT NULL"
  )

  arg_match0(operator, values = allowed)

  mod <- list(operator = operator, value = value)

  class(mod) <- "modifier"

  mod
}

#' @autoglobal
#' @rdname query-modifiers
#' @noRd
greater_than_ <- function(x) {
  check_number_decimal(x)
  modifier_(operator = ">", value = x)
}

#' @autoglobal
#' @rdname query-modifiers
#' @noRd
greater_or_equal_ <- function(x) {
  check_number_decimal(x)
  modifier_(operator = ">=", value = x)
}

#' @autoglobal
#' @rdname query-modifiers
#' @noRd
less_than_ <- function(x) {
  check_number_decimal(x)
  modifier_(operator = "<", value = x)
}

#' @autoglobal
#' @rdname query-modifiers
#' @noRd
less_or_equal_ <- function(x) {
  check_number_decimal(x)
  modifier_(operator = "<=", value = x)
}

#' @autoglobal
#' @rdname query-modifiers
#' @noRd
starts_with_ <- function(x) {
  modifier_(operator = "STARTS_WITH", value = x)
}

#' @autoglobal
#' @rdname query-modifiers
#' @noRd
ends_with_ <- function(x) {
  modifier_(operator = "ENDS_WITH", value = x)
}

#' @autoglobal
#' @rdname query-modifiers
#' @noRd
contains_ <- function(x) {
  modifier_(operator = "CONTAINS", value = x)
}

#' @autoglobal
#' @rdname query-modifiers
#' @noRd
between_ <- function(x) {
  check_number_decimal(x)
  modifier_(operator = "BETWEEN", value = x)
}

#' @autoglobal
#' @rdname query-modifiers
#' @noRd
not_between_ <- function(x) {
  check_number_decimal(x)
  modifier_(operator = "NOT BETWEEN", value = x)
}

#' @autoglobal
#' @rdname query-modifiers
#' @noRd
in_ <- function(x) {
  modifier_(operator = "IN", value = x)
}

#' @autoglobal
#' @rdname query-modifiers
#' @noRd
not_in_ <- function(x) {
  modifier_(operator = "NOT IN", value = x)
}

#' @autoglobal
#' @rdname query-modifiers
#' @noRd
is_ <- function(x) {
  modifier_(operator = "=", value = x)
}

#' @autoglobal
#' @rdname query-modifiers
#' @noRd
is_not_ <- function(x) {
  modifier_(operator = "<>", value = x)
}

#' @autoglobal
#' @rdname query-modifiers
#' @noRd
blank_ <- function(x) {
  modifier_(operator = "IS NULL", value = x)
}

#' @autoglobal
#' @rdname query-modifiers
#' @noRd
not_blank_ <- function(x) {
  modifier_(operator = "IS NOT NULL", value = x)
}
