#' Query Modifiers
#'
#' @description
#' Helpers for use in constructing conditions in queries.
#'
#' @details
#' Query modifiers are a small DSL for use in constructing query conditions,
#' in the __JSON:API__ format.
#'
#' @param x input
#' @param or_equals `<lgl>` append "="; default is `FALSE`
#' @param negate `<lgl>` prepend "NOT"; default is `FALSE`
#' @name query_modifier
#' @returns object of class `<modifier>`
NULL

#' Query Modifier Constructor
#'
#' @param operator `<chr>` comparison operator
#' @param value `<any>` value to compare against
#' @returns object of class `"modifier"`
#'
#' @examples
#' modifier_(
#'   operator = ">",
#'   value    = 1000)
#' @autoglobal
#' @keywords internal
#' @export
modifier_ <- function(operator, value) {

  check_required(value)

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

  structure(
    list(operator = operator, value = value),
    class = "modifier")
}

#' @rdname query_modifier
#' @examples
#' greater_than_(1000)
#' greater_than_(1000, or_equals = TRUE)
#' @autoglobal
#' @export
greater_than_ <- function(x, or_equals = FALSE) {

  check_number_decimal(x)
  check_bool(or_equals)

  modifier_(
    operator = ifelse(!or_equals, ">", ">="),
    value    = x)
}

#' @rdname query_modifier
#' @examples
#' less_than_(1000)
#' less_than_(1000, or_equals = TRUE)
#' @autoglobal
#' @export
less_than_ <- function(x, or_equals = FALSE) {

  check_number_decimal(x)
  check_bool(or_equals)

  modifier_(
    operator = ifelse(!or_equals, "<", "<="),
    value    = x)
}

#' @rdname query_modifier
#' @examples
#' starts_with_("foo")
#' @autoglobal
#' @export
starts_with_ <- function(x) {
  check_character(x, allow_na = FALSE)
  modifier_(operator = "STARTS_WITH", value = x)
}

#' @rdname query_modifier
#' @examples
#' ends_with_("bar")
#' @autoglobal
#' @export
ends_with_ <- function(x) {
  check_character(x, allow_na = FALSE)
  modifier_(operator = "ENDS_WITH", value = x)
}

#' @rdname query_modifier
#' @examples
#' contains_("baz")
#' @autoglobal
#' @export
contains_ <- function(x) {
  check_character(x, allow_na = FALSE)
  modifier_(operator = "CONTAINS", value = x)
}

#' @rdname query_modifier
#' @examples
#' in_(state.abb[10:15])
#' in_(state.abb[10:15], negate = TRUE)
#' @autoglobal
#' @export
in_ <- function(x, negate = FALSE) {

  check_bool(negate)

  modifier_(
    operator = ifelse(!negate, "IN", "NOT IN"),
    value    = x)
}

#' @rdname query_modifier
#' @examples
#' equals_(1000)
#' equals_(1000, negate = TRUE)
#' @autoglobal
#' @export
equals_ <- function(x, negate = FALSE) {

  check_bool(negate)

  modifier_(
    operator = ifelse(!negate, "=", "<>"),
    value    = x)
}

#' @rdname query_modifier
#' @examples
#' is_blank_()
#' is_blank_(negate = TRUE)
#' @autoglobal
#' @export
is_blank_ <- function(negate = FALSE) {

  check_bool(negate)

  modifier_(
    operator = ifelse(!negate, "IS NULL", "IS NOT NULL"),
    value    = NULL)
}

#' @method print modifier
#' @autoglobal
#' @export
print.modifier <- function(x, ...) {
  cli::cli_text(cli::col_cyan("<modifier>"))
  cli::cli_text(c(
    cli::col_silver("operator: "),
    cli::col_yellow(x$operator)
  ))
  if (!is.null(x$value)) {
    cli::cli_text(c(
      cli::col_silver("value:"),
      cli::col_yellow(x$value)
      ))
  }
}

# between_ <- function(x, negate = FALSE) {
#   check_number_decimal(x)
#   check_bool(negate)
#
#   modifier_(
#     operator = ifelse(!negate, "BETWEEN", "NOT BETWEEN"),
#     value    = x)
# }


# `%AND%` <- function(lhs, rhs) {
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
# }

# `%OR%` <- function(lhs, rhs) {
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
# }
