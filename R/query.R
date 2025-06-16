#' @autoglobal
#' @noRd
seq_along0 <- function(x) {
  seq_along(x) - 1
  # 0L:(length(x) - 1L)
}

# x <- list(
# first_name ~ starts_with_("Andr"),
# last_name ~ contains_("Jason"),
# state = ~in_(c("CA", "GA", "NY")),
# npi = 1234567890)

#' @autoglobal
#' @noRd
is_unnamed <- function(x) {
  !have_name(x)
}

#' @autoglobal
#' @noRd
is_formula_full <- function(x) {
  map_lgl(x, is_formula, lhs = TRUE)
}

#' @autoglobal
#' @noRd
is_formula_rhs <- function(x) {
  map_lgl(x, is_formula, lhs = FALSE)
}

#' @autoglobal
#' @noRd
is_unnamed_formula_full <- function(x) {
  is_unnamed(x) & is_formula_full(x)
}

#' @autoglobal
#' @noRd
is_named_formula_rhs <- function(x) {
  have_name(x) & is_formula_rhs(x)
}

#' @autoglobal
#' @noRd
format_unnamed_formula <- function(x) {

  idx <- is_unnamed_formula_full(x)

  if (any(idx)) {

    tmp <- x[idx]

    rhs <- map(tmp, f_rhs)

    lhs <- map(
      tmp, function(x)
        as_string(f_lhs(x))) |>
      list_c()

    x[idx] <- rhs

    names(x)[idx] <- lhs
  }
  x
}

#' @autoglobal
#' @noRd
format_named_formula <- function(x) {

  idx <- is_named_formula_rhs(x)

  if (any(idx)) {

    tmp <- x[idx]

    rhs <- map(tmp, f_rhs)

    x[idx] <- rhs
  }
  x
}

#' @autoglobal
#' @noRd
format_query <- function(x) {

  i <- list_lengths(x) > 1

  if (any(i)) {

    g <- x[i]

    x[i] <- paste0(unlist(g, use.names = FALSE), collapse = ",")

    names(x[i]) <- names(g)

    }

  pr <- format(unlist(x, use.names = FALSE), justify = 'left')

  nm <- format(names(x), justify = 'right')

  glue("{nm} = {pr}")
}

generate_query <- function(a) {

  imap(a, function(x, m) {

    p <- paste0("filter[<<i>>][path]=", m, "&")
    o <- paste0("filter[<<i>>][operator]=", "=", "&")
    v <- unlist(x, use.names = FALSE)

    if (length(v) > 1)

      v <- paste0(
        "filter[<<i>>][value][",
        seq_along(v),
        "]=",
        v,
        "&"
        )

    if (length(v) == 1)
      v <- paste0(
        "filter[<<i>>][value]=",
        v,
        "&"
        )

    c(p, o, v)

  }) |>
    unname() |>
    imap(function(x, idx) {

      greplace(x, "<<i>>", idx) |>
        paste0(collapse = "")

    })

}

#' Format Queries
#' @examples
#' args = list(state = c("GA", "MD"),
#'             last_name = "SMITH",
#'             npi = 1234567890,
#'             PECOS = NULL)
#'
#' query_formatter(args)
#'
#' @param args named `<list>` of `<chr>` arguments
#' @returns `<list>` of formatted query `<exprs>`
#' @autoglobal
#' @rdname query-format
#' @export
query_formatter <- function(args) {

  args <- discard(args, is.null)

  cat(format_query(args), sep = "\n")

  cat("\n")

  qry <- generate_query(args)

  map(
    qry,
    function(x) {
      strsplit(x, "&") |>
        unlist() |>
        append("\n")
      }) |>
    unlist() |>
    cat(sep = "\n")

  invisible(qry)

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

# frm <- list(state ~ in_(c("NY", "CA")))
#
# rlang::f_rhs(frm[[1]]) |>
#   rlang::eval_bare() |>
#   purrr::list_assign(filter = rlang::as_name(rlang::f_lhs(frm[[1]])))
#
#
# frm <- list(state = ~starts_with("NY"))
#
#
# rlang::is_formula(frm$state, lhs = FALSE)
# rlang::is_bare_formula(frm$state, scoped = TRUE, lhs = FALSE)
# rlang::f_rhs(frm)
#
#
# frm2 <- list(
#   state ~ starts_with("NY"),
#   state = ~is_in(c("CA", "GA", "NY")),
#   npi = 1234567890
# )
#
# which_(map_lgl(frm2, rlang::is_formula))
#
# is_formula(frm2$state, lhs = FALSE)
