#' Return the identifier (URL) for a given object
#' @param obj An object of class `<class_endpoint>` or `<class_temporal>`.
#' @autoglobal
#' @keywords internal
#' @export
identifier <- new_generic("identifier", "obj", function(obj) {
  S7_dispatch()
})

method(identifier, class_group) <- function(obj) {
  prop(obj, "members") |> map(identifier)
}

method(identifier, class_endpoint) <- function(obj) {
  prop(obj, "identifier")
}

method(identifier, class_current) <- function(obj) {
  obj
}

method(identifier, class_temporal) <- function(obj) {
  obj
}

#' Return the number of results for a given query
#'
#' @param obj An object of class `<class_endpoint>` or `<class_temporal>`.
#' @param url An object of class `<class_query>`. If `NULL`, the default query for the endpoint is used.
#' @param qstr A `<chr>` string representing the query to be passed to the API. If `NULL`, the default query for the endpoint is used.
#' @param ... Additional arguments passed to the query.
#'
#' @returns An `<int>` representing the number of results found.
#'
#' @examples
#' end_dial <- endpoint("care_dial_end")
#' end_qpp  <- endpoint("quality_payment")
#' end_rhc  <- collection("care_rhc")
#' qry      <- query(
#'               STATE = in_(c("CA", "GA", "NY"), care = TRUE),
#'               `STATE - OWNER` = in_(c("CA", "GA", "NY"), care = TRUE),
#'               .type = "medicare")
#'
#' ncount(end_dial, identifier(end_dial), qry)
#'
#' #ncount(end_rhc, identifier(end_rhc), qry)
#'
#' @autoglobal
#' @export
ncount <- new_generic("ncount", c("obj", "url"), function(obj, url, qstr = NULL, ...) {
  S7_dispatch()
})

method(ncount, list(class_group, class_endpoint)) <- function(
    obj,
    url,
    qstr = NULL) {

  list(
    title   = map(obj@members, \(x) prop(x, "metadata") |> get_elem("title")),
    url     = identifier(obj),
    qstring = qstr@string
  )

  identifier(url) |> walk(\(x) ncount(x, query = qstr@string))
}

method(ncount, list(class_care, class_current)) <- function(
    obj,
    url,
    qstr = NULL) {

  q <- S7_data(url) |> url_modify(query = qstr@string)

  n <- request(q) |>
    req_url_path_append("stats") |>
    perform_simple() |>
    _$data

  list(
    endpoint = prop(obj, "metadata")$title,
    found = n$found_rows,
    total = n$total_rows,
    qstring = utils::URLdecode(q)
  )
}

method(ncount, list(class_care, class_temporal)) <- function(
    obj,
    url,
    qstr = NULL) {

  q <- S7_data(url) |>
    get_elem("identifier") |>
    map_chr(function(x) url_modify(x, query = qstr@string))

  n <- map(q, function(x) {
    request(x) |>
      req_url_path_append("stats") |>
      perform_simple()
  }) |>
    set_names(
      S7_data(url) |>
        get_elem("year"))

  list(
    endpoint = prop(obj, "metadata")$title,
    counts = fastplyr::new_tbl(
      year  = as.integer(names(n)),
      found = as.integer(get_elem(n, "found_rows")),
      total = as.integer(get_elem(n, "total_rows")),
      prop  = found / total,
      qstring = utils::URLdecode(q)
      )
  )

}



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
