# x <- list(
# first_name ~ starts_with_("Andr"),
# last_name ~ contains_("J"),
# state = ~ in_(c("CA", "GA", "NY")),
# country = in_(c("CA", "GA", "NY")),
# state_owner = c("GA", "MD"),
# npi = npi_ex$k,
# npi_owner = npi_ex$k[1],
# ccn = "01256",
# pac = NULL)
#
# x
#
# is_unnamed_formula(x)
# is_named_formula(x)
# !is_unnamed_formula(x) & !is_named_formula(x) & is_length_one(x)
# !is_unnamed_formula(x) & !is_named_formula(x) & is_length_two(x)
#
# check_names_unique(x)
# check_unnamed_formulas(x)

#' @autoglobal
#' @noRd
check_names_unique <- function(x, call = caller_env()) {

  x <- names(x[have_name(x)])

  if (anyDuplicated(x)) {

    i <- which_(fduplicated(x))

    cli_abort(c(
      "x" = "Field{?s} {.field {x[i]}} appea{?rs/rs/r} multiple times."
      ),
      call = call)
  }
}

#' @autoglobal
#' @noRd
check_unnamed_formulas <- function(x, call = caller_env()) {

  if (any(is_unnamed_full_formula(x))) {

    x <- map(x[is_unnamed_full_formula(x)], \(x) as_label(x)) |>
      unlist(use.names = FALSE) |>
      set_names("*")

    cli_abort(c(
      "x" = "{.emph Unnamed formulas} detected:",
      cli::col_yellow(cli::format_bullets_raw(x))),
      call = call)
  }
}

#' @autoglobal
#' @noRd
is_length_one <- function(x) {
  # map_lgl(x, \(x) length(x) == 1L)
  list_lengths(x) == 1L
}

#' @autoglobal
#' @noRd
is_length_two <- function(x) {
  # map_lgl(x, \(x) length(x) > 1L)
  list_lengths(x) > 1L
}

#' @autoglobal
#' @noRd
is_full_formula <- function(x) {
  map_lgl(x, is_formula, lhs = TRUE)
}

#' @autoglobal
#' @noRd
is_rhs_formula <- function(x) {
  map_lgl(x, is_formula, lhs = FALSE)
}

#' @autoglobal
#' @noRd
is_unnamed_full_formula <- function(x) {
  !have_name(x) & is_full_formula(x)
}

#' @autoglobal
#' @noRd
is_named_rhs_formula <- function(x) {
  have_name(x) & is_rhs_formula(x)
}

#' @autoglobal
#' @noRd
convert_unnamed_formula <- function(x) {

  if (any(is_unnamed_full_formula(x))) {

    i <- is_unnamed_full_formula(x)

    tmp <- x[i]

    rhs <- map(tmp, f_rhs)

    lhs <- map(tmp, function(x)
      f_lhs(x) |> as_string()) |>
      list_c()

    x[i] <- rhs

    names(x)[i] <- lhs
  }
  x
}

#' @autoglobal
#' @noRd
convert_named_formula <- function(x) {

  if (any(is_named_rhs_formula(x))) {

    i <- is_named_rhs_formula(x)

    tmp <- x[i]

    rhs <- map(tmp, f_rhs)

    x[i] <- rhs
  }
  x
}

#' @autoglobal
#' @noRd
format_query <- function(x) {

  if (any(is_length_two(x))) {

    two <- x[is_length_two(x)]

    x[is_length_two(x)] <- paste0("[", paste0(unlist(two, use.names = FALSE), collapse = ", "), "]")

    # names(x[is_length_two(x)]) <- names(two)

  }

  if (any(map_lgl(x, is.null))) x[map_lgl(x, is.null)] <- cli::col_red("NULL")

  VALUE <- format(unlist(x, use.names = FALSE), justify = "left")

  FIELD <- format(names(x), justify = "right")

  glue("{FIELD} = {VALUE}")
}

# q2 <- list(
#   `conditions[0][property]` = "state",
#   `conditions[0][operator]` = "IN",
#   `conditions[0][value][1]` = "CA",
#   `conditions[0][value][2]` = "GA",
#   `conditions[0][value][3]` = "NY")
#' @autoglobal
#' @noRd
generate_query <- function(args, type = "def") {
    # encodeString(x, quote = '"')

  args <- discard(args, is.null)

  key <- switch(
    match.arg(type, c("def", "care")),
    care = list(VERB = "filter",     FIELD = "path"),
    def  = list(VERB = "conditions", FIELD = "property"))

  imap(args, function(x, m) {

    p <- paste0(key$VERB, "[<<i>>][", key$FIELD, "]=", m, "&")
    o <- paste0(key$VERB, "[<<i>>][operator]=", "=", "&")
    v <- unlist(x, use.names = FALSE)

    if (length(v) > 1)

      v <- paste0(key$VERB, "[<<i>>][value][", seq_along(v), "]=", v, "&")

    if (length(v) == 1)

      v <- paste0(key$VERB, "[<<i>>][value]=", v, "&")

    c(p, o, v)

  }) |>
    unname() |>
    imap(function(x, idx) {

      greplace(x, "<<i>>", if (type == "def") idx - 1 else idx) |>
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

  cat(format_query(args), sep = "\n")
  cat("\n")

  args <- discard(args, is.null)

  qry <- generate_query(args)

  map(qry, function(x) {
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
