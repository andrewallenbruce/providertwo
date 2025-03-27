#' Process Query Parameters
#'
#' @param arg_names `<chr>` vector of argument names
#' @param field_names `<chr>` vector of API field names
#' @returns `<expr>` of processed query parameters
#'
#' @autoglobal
#' @keywords internal
#' @export
process_params <- function(arg_names, field_names) {

  nms <- set_names(arg_names, field_names)

  parse_expr(
    paste0(
      "list2(",
      glue_collapse(
      glue('{names(nms)} = {unname(nms)}'),
      sep = ", "),
      ")")
    )
}

#' Evaluate Query Parameters
#'
#' @param args `<list>` of query parameters
#' @param fields `<chr>` vector of API field names
#' @returns `<list>` of evaluated query parameters
#'
#' @autoglobal
#' @keywords internal
#' @export
eval_params <- function(args, fields) {

  process_params(args, fields) |>
    eval_bare() |>
    compact()
}

#' Format Public API Queries
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
#' format_query_public(list("NPI" = "1417918293", "PECOS" = NULL))
#'
#' format_query_public(list(NPI = "1417918293", PECOS = "001132"))
#'
#' @autoglobal
#' @export
format_query_public <- function(args, operator = "=") {

  args  <- discard(args, null)

  query <- glue(
    '
  "filter[{i}][path]" = "{PATH}",
  "filter[{i}][operator]" = "{OPERATOR}",
  "filter[{i}][value]" = "{VALUE}"
  ',
    i                 = seq_along(args),
    PATH              = names(args),
    OPERATOR          = operator,
    VALUE             = args) |>
    glue_collapse(sep = ",\n")

  glue('c({query})') |>
    parse_expr() |>
    eval_bare()
}

#' Format Provider API Queries
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
#' format_query_provider(list("NPI" = "1417918293", "PECOS" = NULL))
#'
#' format_query_provider(list(NPI = "1417918293", PECOS = "001132"))
#'
#' @autoglobal
#' @export
format_query_provider <- function(args, operator = "=") {

  args  <- discard(args, null)

  query <- glue(
    '
  "conditions[{i}][property]" = "{PROPERTY}",
  "conditions[{i}][operator]" = "{OPERATOR}",
  "conditions[{i}][value]" = "{VALUE}"
  ',
    i                 = seq_along0(args),
    PROPERTY          = names(args),
    OPERATOR          = operator,
    VALUE             = args) |>
    glue_collapse(sep = ",\n")

  glue('c({query})') |>
    parse_expr() |>
    eval_bare()
}

#' Query Formatting Helpers
#'
#' @name query-helpers
#' @param x      `<chr>`  input
#' @param equals `<lgl>`  append "="; default is `FALSE`
#' @param negate `<lgl>`  prepend "NOT"; default is `FALSE`
#' @returns      `<list>` of query parameters
#' @examplesIf rlang::is_interactive()
#' greater_than(1)
#' starts_with("foo")
#' is_in(state.abb[10:15])
NULL

#' @autoglobal
#' @keywords internal
#' @export
#' @rdname query-helpers
greater_than <- function(x, equals = FALSE) {
  list(field    = character(0L),
       operator = if (equals) ">=" else ">",
       input    = as_character(x))
}

#' @autoglobal
#' @keywords internal
#' @export
#' @rdname query-helpers
less_than <- function(x, equals = FALSE) {
  list(field    = character(0L),
       operator = if (equals) "<=" else "<",
       input    = as_character(x))
}

#' @autoglobal
#' @keywords internal
#' @export
#' @rdname query-helpers
starts_with <- function(x) {
  list(field    = character(0L),
       operator = "STARTS_WITH",
       input    = as_character(x))
}

#' @autoglobal
#' @keywords internal
#' @export
#' @rdname query-helpers
ends_with <- function(x) {
  list(field    = character(0L),
       operator = "ENDS_WITH",
       input    = as_character(x))
}

#' @autoglobal
#' @keywords internal
#' @export
#' @rdname query-helpers
contains <- function(x) {
  list(field    = character(0L),
       operator = "CONTAINS",
       input    = as_character(x))
}

#' @autoglobal
#' @keywords internal
#' @export
#' @rdname query-helpers
between <- function(x, negate = FALSE) {
  list(field    = character(0L),
       operator = if (negate) "NOT BETWEEN" else "BETWEEN",
       input    = as_character(x))
}

#' @autoglobal
#' @keywords internal
#' @export
#' @rdname query-helpers
is_in <- function(x, negate = FALSE) {
  list(field    = character(0L),
       operator = if (negate) "NOT IN" else "IN",
       input    = as_character(x))
}
