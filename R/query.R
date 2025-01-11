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
#'
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
