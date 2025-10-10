# Empty Brackets
#' @autoglobal
#' @noRd
query_default_ARG <- function(args) {

  purrr::imap(args, function(x, N) {

    V <- x@value
    O <- tolower(gsub("_", "+", x@operator, fixed = TRUE))
    N <- gsub(" ", "+", N, fixed = TRUE)
    G <- if (empty(x@member_of)) NULL else x@member_of

    c(
      `if`(
        is.null(G),
        NULL,
        paste0("conditions[<<i>>][memberOf]=", G)),
      paste0("conditions[<<i>>][property]=", N),
      paste0("conditions[<<i>>][operator]=", O),
      `if`(
        length(V) > 1L,
        paste0("conditions[<<i>>][value][]=", V),
        paste0("conditions[<<i>>][value]=", V)))
  }) |>
    unname() |>
    purrr::imap(function(x, idx)
      gsub(x           = x,
           pattern     = "<<i>>",
           replacement = idx - 1,
           fixed       = TRUE)
    ) |>
    purrr::map(paste0, collapse = "&")
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
