#' Return the number of results for a given query
#'
#' @param obj A `<class_endpoint>` or `<class_temporal>` object.
#' @param qry A `<class_query>` object. If `NULL` (the default), no query is used.
#' @returns   A `<list>` with the number of results found, total number of rows, and the URL used for the query.
#' @examplesIf interactive()
#' query_results(endpoint("dial_facility"), new_query(state = any_of(c("GA", "TX"))))
#' query_results(endpoint("lab_fee"))
#' query_results(endpoint("quality_payment"),
#'               new_query(year = 2017:2023,
#'               practice_state_or_us_territory = any_of(c("GA", "TX"))))
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

  x   <- if (is.null(qry))
    standardise(obj = obj)
  else
    standardise(obj = obj, qry = qry)
  url <- if (is.null(qry))
    x$identifier
  else
    paste0(x$identifier, "&", flatten_query(x$query)) |> set_names(names(x$identifier))

  n <- map(url, request) |>
    req_perform_parallel(on_error = "continue") |>
    map(function(x)
      fparse(resp_body_string(x)) |> _[["count"]]) |>
    set_names(names(url))

  fastplyr::new_tbl(
    year  = as.integer(names(n)),
    found = as.integer(n),
    url = utils::URLdecode(url)
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
