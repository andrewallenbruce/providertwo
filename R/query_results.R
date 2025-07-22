#' Return the number of results for a given query
#'
#' @param obj A `<class_endpoint>` or `<class_temporal>` object.
#' @param qry A `<class_query>` object. If `NULL` (the default), no query is used.
#' @returns   A `<list>` with the number of results found, total number of rows, and the URL used for the query.
#' @examplesIf interactive()
#' query_results(endpoint("dial_facility"), query(state = any_of(c("GA", "TX"))))
#' query_results(endpoint("lab_fee_sched"))
#'
#' query_results(
#'    endpoint("quality_payment"),
#'    query(year = 2017:2023,
#'          `practice state or us territory` = any_of(c("GA", "TX"))))
#'
#' @autoglobal
#' @export
query_results <- new_generic("query_nresults", "obj", function(obj, qry = NULL) {
  S7_dispatch()
})

method(query_results, class_catalog) <- function(obj, qry = NULL) {
  prop(obj, "access") |>
    query_results(qry = qry)
}

method(query_results, class_current) <- function(obj, qry = NULL) {

  i <- prop(obj, "identifier")

  if (!is.null(qry)) i <- paste0(i, "&", qry@string$default)

  n <- request(i) |>
    perform_simple() |>
    _$count

  cli::cli_text(cli::col_silver(fmt_int(n)))

  invisible(
    list(
      found = obj@dimensions@rows,
      total = n,
      url   = utils::URLdecode(i))
  )
}

method(query_results, class_temporal) <- function(obj, qry = NULL) {

  i <- prop(obj, "identifier")

  if (!is.null(qry) && "year" %in% names(qry@input)) {

    i <- sbt(i, year %in% qry@input$year)

    if (is_empty(i)) {
      cli::cli_abort(
        c("x" = "{.field year(s)} {.val {qry@input$year}} had {nrow(i)} matches."),
        call = call)
    }
  }

  y <- get_elem(i, "year")
  i <- get_elem(i, "identifier")

  if (!is.null(qry)) i <- paste0(i, "&", qry@string$medicare)

  n <- map(i, function(x) {
    request(x) |>
      perform_simple() |>
      _$count
  }) |>
    set_names(y)

  cli::cli_bullets(
    paste0(
      cli::col_yellow(names(n)), " : ",
      cli::col_silver(map_chr(
        unlist(n, use.names = FALSE), fmt_int)
      )
    )
  )

  invisible(
    fastplyr::new_tbl(
      year  = as.integer(names(n)),
      found = as.integer(n),
      url = utils::URLdecode(i)
    )
  )
}

method(query_results, care_current) <- function(obj, qry = NULL) {

  i <- prop(obj, "identifier")

  if (!is.null(qry)) i <- paste0(i, "&", qry@string$medicare)

  n <- request(i) |>
    req_url_path_append("stats") |>
    perform_simple() |>
    _$data

    list(
      found = n$found_rows,
      total = n$total_rows,
      url   = utils::URLdecode(i))
}

method(query_results, care_temporal) <- function(obj, qry = NULL) {

  i <- prop(obj, "identifier")

  if (!is.null(qry) && "year" %in% names(qry@input)) {

    i <- sbt(i, year %in% qry@input$year)

    if (is_empty(i)) {
      cli::cli_abort(
        c("x" = "{.field year(s)} {.val {qry@input$year}} had {nrow(i)} matches."),
        call = call)
    }
  }

  y <- get_elem(i, "year")
  i <- get_elem(i, "identifier")

  if (!is.null(qry)) i <- paste0(i, "&", qry@string$medicare)

  n <- map(i, function(x) {
    request(x) |>
      req_url_path_append("stats") |>
      perform_simple()
    }) |>
    set_names(y)

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
        url = utils::URLdecode(i)
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
