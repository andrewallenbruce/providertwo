#' Return the number of results for a given query
#'
#' @param obj An object of class `<class_endpoint>` or `<class_temporal>`.
#' @param query A `<class_query>` object. If `NULL` (the default), no query is used.
#' @param ... Additional arguments.
#' @returns A `<list>` with the number of results found, total number of rows, and the URL used for the query.
#'
#' @examples
#' query_results(endpoint("care_dial_end"), query(state = any_of(c("GA", "TX"))))
#'
#' query_results(
#'    endpoint("quality_payment"),
#'    query(year = 2017:2023,
#'          `practice state or us territory` = any_of(c("GA", "TX"))))
#'
#' @autoglobal
#' @export
query_results <- new_generic("query_nresults", "obj", function(obj, query = NULL, ...) {
  S7_dispatch()
})

method(query_results, class_current) <- function(obj, query = NULL) {

  cli::cli_text(cli::col_cyan(obj@metadata$title))

  prop(obj, "access") |>
    query_results(query = query)
}

method(query_results, class_temporal) <- function(obj, query = NULL) {

  cli::cli_text(cli::col_cyan(obj@metadata$title))

  prop(obj, "access") |>
    query_results(query = query)
}

method(query_results, care_current) <- function(obj, query = NULL) {

  q <- prop(obj, "identifier") |>
    url_modify(query = `if`(is.null(query), NULL, query@string$medicare))

  n <- request(q) |>
    req_url_path_append("stats") |>
    perform_simple() |>
    _$data

  cli::cli_text(cli::col_silver(fmt_int(n$found_rows)))

  invisible(
    list(
      found = n$found_rows,
      total = n$total_rows,
      url   = utils::URLdecode(q))
    )
}

method(query_results, care_temporal) <- function(obj, query = NULL) {

  x <- prop(obj, "identifier")

  if (!is.null(query) && "year" %in% names(query@input)) {

    x <- sbt(x, year %iin% query@input$year)

    if (is_empty(x)) {
      cli::cli_abort(
        c("x" = "{.field year(s)} {.val {query@input$year}} had {nrow(x)} matches."),
        call = call)
    }
  }

  q <- get_elem(x, "identifier") |>
    map_chr(function(x)
      url_modify(x, query = `if`(is.null(query), NULL, query@string$medicare)))

  n <- map(q, function(x) {
    request(x) |>
      req_url_path_append("stats") |>
      perform_simple()
    }) |>
    set_names(x$year)

  cli::cli_bullets(
    paste0(
      cli::col_yellow(names(n)), " : ",
      cli::col_silver(map_chr(
        unlist(get_elem(n, "found_rows"), use.names = FALSE), fmt_int)
        )
      )
    )

  invisible(
    fastplyr::new_tbl(
        year  = as.integer(names(n)),
        found = as.integer(get_elem(n, "found_rows")),
        total = as.integer(get_elem(n, "total_rows")),
        url = utils::URLdecode(q)
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
