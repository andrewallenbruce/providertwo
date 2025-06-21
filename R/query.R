# x <- list(
# first_name ~ starts_with_("Andr"),
# last_name ~ contains_("J"),
# state = ~ in_(c("CA", "GA", "NY")),
# state = in_(c("CA", "GA", "NY")),
# npi = npi_ex$k,
# npi = npi_ex$k[1],
# ccn = "01256",
# pac = NULL)
#
# is_unnamed_formula(x)
# is_named_formula(x)
# !is_unnamed_formula(x) & !is_named_formula(x) & is_length_one(x)
# !is_unnamed_formula(x) & !is_named_formula(x) & is_length_two(x)
# check_names_unique(x)
#' @autoglobal
#' @noRd
check_names_unique <- function(x, call = caller_env()) {

  x <- names(x[have_name(x)])

  if (anyDuplicated(x)) {

    i <- which_(fduplicated(x))

    cli_abort(
      c("x" = "Field{?s} {.field {x[i]}} appea{?rs/rs/r} multiple times."),
      call = call)
  }
}

#' @autoglobal
#' @noRd
is_length_one <- function(x) {
  # list_lengths(x) == 1L
  map_lgl(x, \(x) length(x) == 1L)
}

#' @autoglobal
#' @noRd
is_length_two <- function(x) {
  map_lgl(x, \(x) length(x) > 1L)
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
is_unnamed_formula <- function(x) {
  !have_name(x) & is_formula_full(x)
}

#' @autoglobal
#' @noRd
is_named_formula <- function(x) {
  have_name(x) & is_formula_rhs(x)
}

#' @autoglobal
#' @noRd
convert_unnamed_formula <- function(x) {

  idx <- is_unnamed_formula(x)

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
convert_named_formula <- function(x) {

  idx <- is_named_formula(x)

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

    x[i] <- paste0(
      "[",
      paste0(
        unlist(g, use.names = FALSE),
        collapse = " ,"),
      "]")

    names(x[i]) <- names(g)

    }

  pr <- format(unlist(x, use.names = FALSE), justify = 'left')

  nm <- format(names(x), justify = 'right')

  glue("{nm} = {pr}")
}

generate_query <- function(a) {

  imap(a, function(x, m) {

    # encodeString(x, quote = '"')

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

#' @autoglobal
#' @noRd
seq_along0 <- function(x) {
  seq_along(x) - 1
  # 0L:(length(x) - 1L)
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
