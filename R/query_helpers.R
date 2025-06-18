#' Query Formatting Helpers
#'
#' @name query-helpers
#' @param x      `<chr>`  input
#' @param equals `<lgl>`  append "="; default is `FALSE`
#' @param negate `<lgl>`  prepend "NOT"; default is `FALSE`
#' @returns      `<list>` of query parameters
#' @examplesIf rlang::is_interactive()
#' greater_than_(1)
#' starts_with_("foo")
#' in_(state.abb[10:15])
#' @noRd
NULL

#' @autoglobal
#' @rdname query-helpers
#' @noRd
greater_than_ <- function(x, or_equals = FALSE) {
  list(filter = character(0L),
       operator = if (or_equals) ">=" else ">",
       value = as.character(x))
}

#' @autoglobal
#' @rdname query-helpers
#' @noRd
less_than_ <- function(x, or_equals = FALSE) {
  list(filter = character(0L),
       operator = if (or_equals) "<=" else "<",
       value = as.character(x))
}

#' @autoglobal
#' @rdname query-helpers
#' @noRd
starts_with_ <- function(x) {
  list(filter = character(0L),
       operator = "STARTS_WITH",
       value = as.character(x))
}

#' @autoglobal
#' @rdname query-helpers
#' @noRd
ends_with_ <- function(x) {
  list(filter = character(0L),
       operator = "ENDS_WITH",
       value = as.character(x))
}

#' @autoglobal
#' @rdname query-helpers
#' @noRd
contains_ <- function(x) {
  list(filter = character(0L),
       operator = "CONTAINS",
       value = as_character(x))
}

#' @autoglobal
#' @rdname query-helpers
#' @noRd
between_ <- function(x, not = FALSE) {
  list(filter = character(0L),
       operator = if (not) "NOT BETWEEN" else "BETWEEN",
       value = as.character(x))
}

#' @autoglobal
#' @rdname query-helpers
#' @noRd
in_ <- function(x, not = FALSE) {
  list(filter = character(0L),
       operator = if (not) "NOT IN" else "IN",
       value = as.character(x))
}
