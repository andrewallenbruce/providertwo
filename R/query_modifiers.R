#' Query Modifiers
#'
#' @param operator `<chr>` comparison operator
#' @param value `<any>` value to compare against
#' @returns `<list>` of class `"modifier"`
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

#' Numeric Modifiers
#'
#' @param x `<num>`  input
#' @param or_equals `<lgl>` append "="; default is `FALSE`
#' @returns `<list>` of class `"modifier"`
#' @examples
#' greater_than_(1000)
#' greater_than_(1000, or_equals = TRUE)
#'
#' less_than_(1000)
#' less_than_(1000, or_equals = TRUE)
#' @autoglobal
#' @family modifiers
#' @export
greater_than_ <- function(x, or_equals = FALSE) {

  check_number_decimal(x)
  check_bool(or_equals)

  modifier_(
    operator = ifelse(!or_equals, ">", ">="),
    value    = x)
}

#' @rdname greater_than_
#' @autoglobal
#' @family modifiers
#' @export
less_than_ <- function(x, or_equals = FALSE) {

  check_number_decimal(x)
  check_bool(or_equals)

  modifier_(
    operator = ifelse(!or_equals, "<", "<="),
    value    = x)
}

#' Character Modifiers
#'
#' @param x `<chr>` input
#' @returns `<list>` of class `"modifier"`
#' @examples
#' starts_with_("foo")
#' ends_with_("bar")
#' contains_("baz")
#' @autoglobal
#' @family modifiers
#' @export
starts_with_ <- function(x) {
  check_character(x, allow_na = FALSE)
  modifier_(operator = "STARTS_WITH", value = x)
}

#' @rdname starts_with_
#' @autoglobal
#' @family modifiers
#' @export
ends_with_ <- function(x) {
  check_character(x, allow_na = FALSE)
  modifier_(operator = "ENDS_WITH", value = x)
}

#' @rdname starts_with_
#' @autoglobal
#' @family modifiers
#' @export
contains_ <- function(x) {
  check_character(x, allow_na = FALSE)
  modifier_(operator = "CONTAINS", value = x)
}

#' General Modifiers
#'
#' @param x `<chr>` input
#' @param negate `<lgl>` prepend "NOT"; default is `FALSE`
#' @returns `<list>` of class `"modifier"`
#' @examples
#' in_(state.abb[10:15])
#' in_(state.abb[10:15], negate = TRUE)
#'
#' equals_(1000)
#' equals_(1000, negate = TRUE)
#'
#' is_blank_()
#' is_blank_(negate = TRUE)
#' @autoglobal
#' @family modifiers
#' @export
in_ <- function(x, negate = FALSE) {

  check_bool(negate)

  modifier_(
    operator = ifelse(!negate, "IN", "NOT IN"),
    value    = x)
}

#' @rdname in_
#' @autoglobal
#' @family modifiers
#' @export
equals_ <- function(x, negate = FALSE) {

  check_bool(negate)

  modifier_(
    operator = ifelse(!negate, "=", "<>"),
    value    = x)
}

#' @rdname in_
#' @autoglobal
#' @family modifiers
#' @export
is_blank_ <- function(negate = FALSE) {

  check_bool(negate)

  modifier_(
    operator = ifelse(!negate, "IS NULL", "IS NOT NULL"),
    value    = NULL)
}

#' @autoglobal
#' @family modifiers
#' @export
print.modifier <- function(x, ...) {
  cli::cli_text(cli::col_cyan("<modifier>"))
  cli::cli_text("{cli::col_silver('operator:')} {cli::col_yellow(glue::backtick(x$operator))}")
  if (!is.null(x$value)) {
    cli::cli_text("{cli::col_silver('value:')} {cli::col_yellow(x$value)}")
  } else {
    cli::cli_text("{cli::col_silver('value:')} {cli::col_red('NULL')}")
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
