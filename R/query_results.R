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
query_results <- new_generic("query_results", "obj", function(obj, qry = NULL) {
  S7_dispatch()
})

method(query_results, class_catalog) <- function(obj, qry = NULL) {
  prop(obj, "access") |>
    query_results(qry = qry)
}

method(query_results, class_current) <- function(obj, qry = NULL) {
  i <- prop(obj, "identifier")

  if (!is.null(qry))
    i <- paste0(i, "&", qry@string$default)

  n <- request(i) |>
    perform_simple() |>
    _$count

  list(
    found = obj@dimensions@rows,
    total = n,
    url   = utils::URLdecode(i)
  )
}

method(query_results, class_temporal) <- function(obj, qry = NULL) {
  i <- prop(obj, "identifier")

  if (!is.null(qry) && "year" %in% names(qry@input)) {
    i <- sbt(i, year %in% qry@input$year)

    if (is_empty(i)) {
      cli::cli_warn(c(
        "{.field year(s)} {.val {qry@input$year}} had {nrow(i)} matches."
      ),
      call = call)
      invisible(NULL)
    }
  }

  y <- get_elem(i, "year")
  i <- get_elem(i, "identifier")

  if (!is.null(qry))
    i <- paste0(i, "&", qry@string$medicare)

  n <- map(i, function(x) {
    request(x) |>
      perform_simple() |>
      _$count
  }) |>
    set_names(y)

  fastplyr::new_tbl(
    year  = as.integer(names(n)),
    found = as.integer(n),
    url = utils::URLdecode(i)
  )
}

method(query_results, care_current) <- function(obj, qry = NULL) {
  i <- prop(obj, "identifier")

  if (!is.null(qry))
    i <- paste0(i, "&", qry@string$medicare)

  n <- request(i) |>
    req_url_path_append("stats") |>
    perform_simple() |>
    _$data

  list(
    found = n$found_rows,
    total = n$total_rows,
    url   = utils::URLdecode(i)
  )
}

method(query_results, care_temporal) <- function(obj, qry = NULL) {
  i <- prop(obj, "identifier")

  if (!is.null(qry) && "year" %in% names(qry@input)) {
    i <- sbt(i, year %in% qry@input$year)

    if (is_empty(i)) {
      cli::cli_abort(c("x" = "{.field year(s)} {.val {qry@input$year}} had {nrow(i)} matches."),
                     call = call)
    }
  }

  y <- get_elem(i, "year")
  i <- get_elem(i, "identifier")

  if (!is.null(qry))
    i <- paste0(i, "&", qry@string$medicare)

  n <- map(i, function(x) {
    request(x) |>
      req_url_path_append("stats") |>
      perform_simple()
  }) |>
    set_names(y)


  fastplyr::new_tbl(
    year  = as.integer(names(n)),
    found = as.integer(get_elem(n, "found_rows")),
    total = as.integer(get_elem(n, "total_rows")),
    url = utils::URLdecode(i)
  )
}
