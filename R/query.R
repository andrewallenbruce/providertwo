#' @autoglobal
#' @noRd
seq_along0 <- function(x) {
  seq_along(x) - 1
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
#' format_query_care(list("NPI" = "1417918293", "PECOS" = NULL))
#'
#' format_query_care(list(NPI = "1417918293", PECOS = "001132"))
#'
#' format_query_pro(list("NPI" = "1417918293", "PECOS" = NULL))
#'
#' format_query_pro(list(NPI = "1417918293", PECOS = "001132"))
#'
#' @name query-format
NULL

#' @autoglobal
#' @export
#' @rdname query-format
format_query_care <- function(args, operator = "=") {

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

#' @autoglobal
#' @export
#' @rdname query-format
format_query_pro <- function(args, operator = "=") {

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

#' @autoglobal
#' @noRd
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

#' @autoglobal
#' @noRd
eval_params <- function(args, fields) {

  process_params(args, fields) |>
    eval_bare() |>
    compact()
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
#' @noRd
NULL

#' @autoglobal
#' @rdname query-helpers
#' @noRd
greater_than <- function(x, equals = FALSE) {
  list(field    = character(0L),
       operator = if (equals) ">=" else ">",
       input    = as.character(x))
}

#' @autoglobal
#' @rdname query-helpers
#' @noRd
less_than <- function(x, equals = FALSE) {
  list(field    = character(0L),
       operator = if (equals) "<=" else "<",
       input    = as.character(x))
}

#' @autoglobal
#' @rdname query-helpers
#' @noRd
starts_with <- function(x) {
  list(field    = character(0L),
       operator = "STARTS_WITH",
       input    = as.character(x))
}

#' @autoglobal
#' @rdname query-helpers
#' @noRd
ends_with <- function(x) {
  list(field    = character(0L),
       operator = "ENDS_WITH",
       input    = as.character(x))
}

#' @autoglobal
#' @rdname query-helpers
#' @noRd
contains <- function(x) {
  list(field    = character(0L),
       operator = "CONTAINS",
       input    = as_character(x))
}

#' @autoglobal
#' @rdname query-helpers
#' @noRd
between <- function(x, negate = FALSE) {
  list(field    = character(0L),
       operator = if (negate) "NOT BETWEEN" else "BETWEEN",
       input    = as.character(x))
}

#' @autoglobal
#' @rdname query-helpers
#' @noRd
is_in <- function(x, negate = FALSE) {
  list(field    = character(0L),
       operator = if (negate) "NOT IN" else "IN",
       input    = as.character(x))
}

# query_in_main <- function(args,
#                           operator = c("IN", "NOT IN")) {
#
#   operator <- match.arg(operator)
#   args     <- args[cheapr::which_(cheapr::lengths_(args, names = TRUE) > 1)]
#
#   glue::as_glue(
#     c(
#       glue::glue(
#         "filter[{seq_along(args)}][condition][path]={names(args)}
#          filter[{seq_along(args)}][condition][operator]={operator}"
#       ),
#       glue::glue(
#         "filter[{seq_along(args)}][condition][value]",
#         "[{seq_along(delist(args))}]={delist(args)}"
#       )
#     )
#   )
# }
#
# query_in_main(args = list(NPI = npi_ex))
#
# query_in_pro <- function(args,
#                          operator = c("IN", "NOT IN")) {
#
#   operator <- match.arg(operator)
#   args     <- args[cheapr::which_(cheapr::lengths_(args, names = TRUE) > 1)]
#
#   glue::as_glue(
#     c(
#       glue::glue(
#         "conditions[{seq_along0(args)}][property]={names(args)},\n",
#         "conditions[{seq_along0(args)}][operator]={operator}"
#       ),
#       glue::glue(
#         "conditions[{seq_along0(args)}][value]",
#         "[{seq_along(delist(args))}]={delist(args)}"
#       )
#     )
#   )
# }
#
#
# query_in_pro(args = list(NPI = npi_ex)) |>
#   glue_collapse(sep = ",\n") |>
#   c() |>
#   parse_expr() |>
#   eval_bare()
#
# glue('c({query})') |>
#   parse_expr() |>
#   eval_bare()
