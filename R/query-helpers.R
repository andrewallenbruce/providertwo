#' Query Formatting Helpers
#'
#' @name query-helpers
#'
#' @param x `<chr>` function argument input
#'
#' @returns `<list>` of function argument and operator
#'
#' @examples
#' greater_than(1)
#'
#' starts_with("foo")
#'
#' is_in(state.abb[10:15])
NULL

#' @rdname query-helpers
#' @autoglobal
#' @export
greater_than <- \(x) {
  list(path      = NA_character_,
       operator  = ">",
       value     = x)
}

#' @rdname query-helpers
#' @autoglobal
#' @export
greater_equal <- \(x) {
  list(path      = NA_character_,
       operator  = ">=",
       value     = x)
}

#' @rdname query-helpers
#' @autoglobal
#' @export
less_than <- \(x) {
  list(path      = NA_character_,
       operator  = "<",
       value     = x)
}

#' @rdname query-helpers
#' @autoglobal
#' @export
less_equal <- \(x) {
  list(path      = NA_character_,
       operator  = "<=",
       value     = x)
}

#' @rdname query-helpers
#' @autoglobal
#' @export
starts_with <- \(x) {
  list(path      = NA_character_,
       operator  = "STARTS_WITH",
       value     = x)
}

#' @rdname query-helpers
#' @autoglobal
#' @export
ends_with <- \(x) {
  list(path      = NA_character_,
       operator  = "ENDS_WITH",
       value     = x)
}

#' @rdname query-helpers
#' @autoglobal
#' @export
includes <- \(x) {
  list(path      = NA_character_,
       operator  = "CONTAINS",
       value     = x)
}

#' @rdname query-helpers
#' @autoglobal
#' @export
inside <- \(x) {
  list(path      = NA_character_,
       operator  = "BETWEEN",
       value     = x)
}

#' @rdname query-helpers
#' @autoglobal
#' @export
outside <- \(x) {
  list(path      = NA_character_,
       operator  = "NOT BETWEEN",
       value     = x)
}

#' @rdname query-helpers
#' @autoglobal
#' @export
is_in <- \(x) {
  list(path      = NA_character_,
       operator  = "IN",
       value     = x)
}

#' @rdname query-helpers
#' @autoglobal
#' @export
not_in <- \(x) {
  list(path      = NA_character_,
       operator  = "NOT IN",
       value     = x)
}
