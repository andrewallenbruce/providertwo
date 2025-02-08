#' Format API Queries
#'
#' @param args named `<list>` or vector of `<chr>` arguments
#'
#' @param operator `<chr>` logical operator; acceptable options are:
#'                         `=`, `>=`, `<=`, `>`, `<`, `<>`, `STARTS_WITH`,
#'                         `ENDS_WITH`, `CONTAINS`, `IN`, `NOT IN`, `BETWEEN`,
#'                         `NOT BETWEEN`, `IS NULL`, `IS NOT NULL`
#'
#' @returns `<list>` of formatted query `<exprs>`
#'
#' @examples
#' format_query(list("NPI" = "1417918293", "PECOS_ASCT_CNTL_ID" = NULL))
#'
#' format_query(c("NPI" = "1417918293", "PECOS_ASCT_CNTL_ID" = NULL))
#'
#' @autoglobal
#' @keywords internal
#' @export
format_query <- \(args, operator = "=") {

  args  <- discard(args, null)

  query <- glue(
    '
  "filter[fID{fID}][path]" = "{PATH}",
  "filter[fID{fID}][operator]" = "{OPERATOR}",
  "filter[fID{fID}][value]" = "{VALUE}"
  ',
    fID      = seq_along(args),
    PATH     = names(args),
    OPERATOR = operator,
    VALUE    = args) |>
    glue_collapse(sep = ",\n")

  glue('c({query})') |>
    parse_expr() |>
    eval_bare()
}

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
#' @keywords internal
#' @export
greater_than <- \(x) {
  list(path      = NA_character_,
       operator  = ">",
       value     = x)
}

#' @rdname query-helpers
#' @autoglobal
#' @keywords internal
#' @export
greater_equal <- \(x) {
  list(path      = NA_character_,
       operator  = ">=",
       value     = x)
}

#' @rdname query-helpers
#' @autoglobal
#' @keywords internal
#' @export
less_than <- \(x) {
  list(path      = NA_character_,
       operator  = "<",
       value     = x)
}

#' @rdname query-helpers
#' @autoglobal
#' @keywords internal
#' @export
less_equal <- \(x) {
  list(path      = NA_character_,
       operator  = "<=",
       value     = x)
}

#' @rdname query-helpers
#' @autoglobal
#' @keywords internal
#' @export
starts_with <- \(x) {
  list(path      = NA_character_,
       operator  = "STARTS_WITH",
       value     = x)
}

#' @rdname query-helpers
#' @autoglobal
#' @keywords internal
#' @export
ends_with <- \(x) {
  list(path      = NA_character_,
       operator  = "ENDS_WITH",
       value     = x)
}

#' @rdname query-helpers
#' @autoglobal
#' @keywords internal
#' @export
includes <- \(x) {
  list(path      = NA_character_,
       operator  = "CONTAINS",
       value     = x)
}

#' @rdname query-helpers
#' @autoglobal
#' @keywords internal
#' @export
inside <- \(x) {
  list(path      = NA_character_,
       operator  = "BETWEEN",
       value     = x)
}

#' @rdname query-helpers
#' @autoglobal
#' @keywords internal
#' @export
outside <- \(x) {
  list(path      = NA_character_,
       operator  = "NOT BETWEEN",
       value     = x)
}

#' @rdname query-helpers
#' @autoglobal
#' @keywords internal
#' @export
is_in <- \(x) {
  list(path      = NA_character_,
       operator  = "IN",
       value     = x)
}

#' @rdname query-helpers
#' @autoglobal
#' @keywords internal
#' @export
not_in <- \(x) {
  list(path      = NA_character_,
       operator  = "NOT IN",
       value     = x)
}
