#' Query Formatting Helpers
#'
#' @name query-helpers
#'
#' @param x `<chr>` function argument input
#'
#' @returns `<list>` of function argument and operator
#'
#' @examples
#' greaterthan(1)
#'
#' startswith("foo")
#'
#' is_in(state.abb[10:15])
NULL

#' @rdname query-helpers
#' @autoglobal
#' @export
greaterthan <- \(x) {
  list(arg = x, op  = ">")
}

#' @rdname query-helpers
#' @autoglobal
#' @export
greaterequal <- \(x) {
  list(arg = x, op  = ">=")
}

#' @rdname query-helpers
#' @autoglobal
#' @export
lessthan <- \(x) {
  list(arg = x, op  = "<")
}

#' @rdname query-helpers
#' @autoglobal
#' @export
lessequal <- \(x) {
  list(arg = x, op  = "<=")
}

#' @rdname query-helpers
#' @autoglobal
#' @export
startswith <- \(x) {
  list(arg = x, op  = "STARTS_WITH")
}

#' @rdname query-helpers
#' @autoglobal
#' @export
endswith <- \(x) {
  list(arg = x, op  = "ENDS_WITH")
}

#' @rdname query-helpers
#' @autoglobal
#' @export
includes <- \(x) {
  list(arg = x, op  = "CONTAINS")
}

#' @rdname query-helpers
#' @autoglobal
#' @export
inside <- \(x) {
  list(arg = x, op  = "BETWEEN")
}

#' @rdname query-helpers
#' @autoglobal
#' @export
outside <- \(x) {
  list(arg = x, op  = "NOT BETWEEN")
}

#' @rdname query-helpers
#' @autoglobal
#' @export
is_in <- \(x) {
  list(arg = x, op  = "IN")
}

#' @rdname query-helpers
#' @autoglobal
#' @export
not_in <- \(x) {
  list(arg = x, op  = "NOT IN")
}
