#' Query Modifiers
#'
#' @name query-modifiers
#' @param x         `<chr>`  input
#' @param or_equals `<lgl>`  append "="; default is `FALSE`
#' @param negate    `<lgl>`  prepend "NOT"; default is `FALSE`
#' @returns         `<list>` of class `"modifier"`
#' @examples
#' starts_with_("foo")
#' ends_with_("bar")
#' contains_("baz")
#'
#' in_(state.abb[10:15])
#' in_(state.abb[10:15], negate = TRUE)
#'
#' equals_(1000)
#' equals_(1000, negate = TRUE)
#'
#' greater_than_(1000)
#' greater_than_(1000, or_equals = TRUE)
#'
#' less_than_(1000)
#' less_than_(1000, or_equals = TRUE)
#'
#' is_blank_()
#' is_blank_(negate = TRUE)
NULL

#' @autoglobal
#' @rdname query-modifiers
#' @keywords internal
#' @export
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
#' @export
greater_than_ <- function(x, or_equals = FALSE) {

  check_number_decimal(x)
  check_bool(or_equals)

  modifier_(
    operator = ifelse(!or_equals, ">", ">="),
    value    = x)
}

#' @autoglobal
#' @rdname query-modifiers
#' @export
less_than_ <- function(x, or_equals = FALSE) {

  check_number_decimal(x)
  check_bool(or_equals)

  modifier_(
    operator = ifelse(!or_equals, "<", "<="),
    value    = x)
}

#' @autoglobal
#' @rdname query-modifiers
#' @export
starts_with_ <- function(x) {
  modifier_(operator = "STARTS_WITH", value = x)
}

#' @autoglobal
#' @rdname query-modifiers
#' @export
ends_with_ <- function(x) {
  modifier_(operator = "ENDS_WITH", value = x)
}

#' @autoglobal
#' @rdname query-modifiers
#' @export
contains_ <- function(x) {
  modifier_(operator = "CONTAINS", value = x)
}

# between_ <- function(x, negate = FALSE) {
#   check_number_decimal(x)
#   check_bool(negate)
#
#   modifier_(
#     operator = ifelse(!negate, "BETWEEN", "NOT BETWEEN"),
#     value    = x)
# }

#' @autoglobal
#' @rdname query-modifiers
#' @export
in_ <- function(x, negate = FALSE) {

  check_bool(negate)

  modifier_(
    operator = ifelse(!negate, "IN", "NOT IN"),
    value    = x)
}

#' @autoglobal
#' @rdname query-modifiers
#' @export
equals_ <- function(x, negate = FALSE) {

  check_bool(negate)

  modifier_(
    operator = ifelse(!negate, "=", "<>"),
    value    = x)
}

#' @autoglobal
#' @rdname query-modifiers
#' @export
is_blank_ <- function(negate = FALSE) {

  check_bool(negate)

  modifier_(
    operator = ifelse(!negate, "IS NULL", "IS NOT NULL"),
    value    = NULL)
}

#' @autoglobal
#' @noRd
`%AND%` <- function(lhs, rhs) {
  # group AND
  #
  # filter[g1][group][conjunction]=AND
  # filter[1][condition][memberOf]=g1
  # filter[2][condition][memberOf]=g1
  #
  # filter[1][condition][path]=first_name
  # filter[1][condition][operator]==
  # filter[1][condition][value]=Janis
  #
  # filter[2][condition][path]=last_name
  # filter[2][condition][operator]=STARTS_WITH
  # filter[2][condition][value]=J
}

#' @autoglobal
#' @noRd
`%OR%` <- function(lhs, rhs) {
  # group OR
  #
  # filter[g1][group][conjunction]=OR
  # filter[1][condition][memberOf]=g1
  # filter[2][condition][memberOf]=g1
  #
  # filter[1][condition][path]=PROVIDER_TYPE_DESC
  # filter[1][condition][operator]=CONTAINS
  # filter[1][condition][value]=PRACTITIONER
  #
  # filter[2][condition][path]=STATE_CD
  # filter[2][condition][operator]==
  # filter[2][condition][value]=MD
}
