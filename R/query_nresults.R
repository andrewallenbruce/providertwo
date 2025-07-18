#' Return the number of results for a given query
#'
#' @param obj An object of class `<class_endpoint>` or `<class_temporal>`.
#' @param q A `<chr>` string representing the query to be passed to the API. If `NULL`, the default query for the endpoint is used.
#' @param ... Additional arguments passed to the query.
#'
#' @returns An `<int>` representing the number of results found.
#'
#' @examplesIf interactive()
#' dial <- endpoint("care_dial_end")
#' qual <- endpoint("quality_payment")
#' rhc  <- collection("care_rhc")
#' qry  <- query(
#'               STATE = in_(c("CA", "GA", "NY"), care = TRUE),
#'               `STATE - OWNER` = in_(c("CA", "GA", "NY"), care = TRUE),
#'               .type = "medicare")
#'
#' query_nresults(end_dial, identifier(end_dial), qry)
#'
#' query_nresults(end_rhc, identifier(end_rhc), qry)
#'
#' @autoglobal
#' @export
query_nresults <- new_generic("query_nresults", "obj", function(obj, q = NULL, ...) {
  S7_dispatch()
})
#
# method(query_nresults, list(class_group, class_endpoint)) <- function(obj, q = NULL) {
#
#   list(
#     title   = map(obj@members, \(x) prop(x, "metadata") |> get_elem("title")),
#     url     = identifier(obj),
#     qstring = qstr@string
#   )
#
#   identifier(url) |> walk(\(x) ncount(x, query = qstr@string))
# }
#
# method(query_nresults, list(class_care, class_current)) <- function(obj, q = NULL) {
#
#   q <- S7_data(url) |> url_modify(query = qstr@string)
#
#   n <- request(q) |>
#     req_url_path_append("stats") |>
#     perform_simple() |>
#     _$data
#
#   list(
#     endpoint = prop(obj, "metadata")$title,
#     found = n$found_rows,
#     total = n$total_rows,
#     qstring = utils::URLdecode(q)
#   )
# }
#
# method(query_nresults, list(class_care, class_temporal)) <- function(obj, q = NULL) {
#
#   q <- S7_data(url) |>
#     get_elem("identifier") |>
#     map_chr(function(x) url_modify(x, query = qstr@string))
#
#   n <- map(q, function(x) {
#     request(x) |>
#       req_url_path_append("stats") |>
#       perform_simple()
#   }) |>
#     set_names(
#       S7_data(url) |>
#         get_elem("year"))
#
#   list(
#     endpoint = prop(obj, "metadata")$title,
#     counts = fastplyr::new_tbl(
#       year  = as.integer(names(n)),
#       found = as.integer(get_elem(n, "found_rows")),
#       total = as.integer(get_elem(n, "total_rows")),
#       prop  = found / total,
#       qstring = utils::URLdecode(q)
#       )
#   )
#
# }



# cli::cli_text(
#   cli::col_black(cli::style_bold("Results ")),
#   cli::col_silver(paste0(strrep(cli::symbol$stop, 8)))
# )
#
# cli::cli_text(
#   cli::col_silver(fmt_int(n$found_rows)),
#   " (",
#   roundup(n$found_rows / n$total_rows),
#   " %)"
# )
#
# cli::cli_text(
#   cli::col_silver(paste0(strrep(cli::symbol$double_line, 8))),
#   cli::style_italic(" Query"))
#
# cli::cli_bullets(
#   paste(
#     cli::style_bold(cli::col_yellow(names(query@input))),
#     cli::col_silver(cli::symbol$double_line),
#     cli::col_yellow(query@input),
#     sep = " "
#   ))
#
# cli::cli_text(
#   cli::col_silver(paste0(strrep(cli::symbol$double_line, 8))),
#   cli::style_italic(" Endpoint"))
#
# cli::cli_text(cli::col_cyan(obj@metadata$title))
#
# invisible(q)
