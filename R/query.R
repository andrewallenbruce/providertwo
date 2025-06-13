#' @autoglobal
#' @noRd
seq_along0 <- function(x) {
  seq_along(x) - 1
  # 0L:(length(x) - 1L)
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
#' @examplesIf rlang::is_interactive()
#' format_query_care(list("NPI" = "1417918293", "PECOS" = NULL))
#'
#' format_query_care(list(NPI = "1417918293", PECOS = "001132"))
#'
#' format_query_pro(list("NPI" = "1417918293", "PECOS" = NULL))
#'
#' format_query_pro(list(NPI = "1417918293", PECOS = "001132"))
#'
#' @name query-format
#' @noRd
NULL

#' @autoglobal
#' @rdname query-format
#' @noRd
format_query_care <- function(args, operator = "=") {

  args  <- discard(args, is.null)

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
#' @rdname query-format
#' @noRd
format_query_pro <- function(args, operator = "=") {

  args  <- discard(args, is.null)

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

# args = list(
#   state = c("GA", "MD"),
#   last_name = "SMITH",
#   npi = 1234567890)

#' @autoglobal
#' @noRd
query_formatter <- function(args) {

  fmt_q <- function(x, m) {

    p <- paste0("filter[<<i>>][path]=", m, "&")
    o <- paste0("filter[<<i>>][operator]=", "=", "&")
    v <- unlist(x, use.names = FALSE)
    v <- if (length(v) > 1)
      paste0("filter[<<i>>][value][", seq_along(v), "]=", v, "&")
    else
      paste0("filter[<<i>>][value]=", v, "&")

    c(p, o, v)
  }

  imap(args, fmt_q) |>
    unname() |>
    imap(\(x, idx) greplace(x, "<<i>>", idx) |> paste0(collapse = ""))
}
