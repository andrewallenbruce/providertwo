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

#' Retrieve Fields
#'
#' @param x `<S7_class>` Dataset
#' @returns `<chr>` vector of API field names
#'
#' @autoglobal
#' @keywords internal
#' @export
fields <- function(x) {
  prop(prop(x, "identifier"), "fields")
}

#' Retrieve Identifier URL
#'
#' @param x `<S7_class>` Dataset
#' @returns `<chr>` Identifier URL
#'
#' @autoglobal
#' @keywords internal
#' @export
endpoint <- function(x) {
  prop(prop(x, "identifier"), "url")
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
#' @keywords internal
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
#' @keywords internal
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
