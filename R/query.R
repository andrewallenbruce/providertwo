#' Format API Queries
#'
#' @param args `<list>` of `<chr>` arguments
#'
#' @param operator `<chr>` logical operator to use; options are "=" (default),
#'   ">=", "<=", ">", "<", "<>", "STARTS_WITH", "ENDS_WITH", "CONTAINS", "IN",
#'   "NOT IN", "BETWEEN", "NOT BETWEEN", "IS NULL", "IS NOT NULL"
#'
#' @returns `<list>` of formatted query `<expr>`
#'
#' @examples
#' format_query(list("NPI" = "1417918293", "PECOS_ASCT_CNTL_ID" = NULL))
#'
#' @autoglobal
#'
#' @export
format_query <- \(args, operator = "=") {

  args <- purrr::discard(args, null)

  query <- glue::glue(
    '
  "filter[fID{fID}][path]" = "{PATH}",
  "filter[fID{fID}][operator]" = "{OPERATOR}",
  "filter[fID{fID}][value]" = "{VALUE}"
  ',
    fID      = seq_along(args),
    PATH     = names(args),
    OPERATOR = operator,
    VALUE    = args
  ) |>
    glue::glue_collapse(sep = ",\n")

  glue::glue('c({query})') |>
    rlang::parse_expr() |>
    rlang::eval_bare()
}
