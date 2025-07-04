#' @include classes.R
NULL

#' @name base_request
#' @title Create a new `<httr2_request>` by class
#' @param x A `class_endpoint`, `class_temporal` or `class_group` object
#' @param ... Additional arguments
#' @returns An `<httr2_request>` object or list of `<httr2_request>` objects
#' @examplesIf interactive()
#' endpoint("quality_payment")
#' collection("care_hospital")
#' endpoint("pdc_affiliations")
#' endpoint("mlr_summary")
#' endpoint("hgov_catastrophic")
#' endpoint("hgov_mlr")
#' @autoglobal
#' @export
base_request <- new_generic("base_request", "x")

method(base_request, class_endpoint) <- function(x, query = NULL, years = NULL) {
  identifier_(x) |>
    request() |>
    req_throttle(capacity = 30, fill_time_s = 60) |>
    req_url_query(splice(query)) |>
    req_error(is_error = ~ FALSE)
}

method(base_request, class_temporal) <- function(x, query = NULL, years = NULL) {
  if (is_empty(years)) {
    return(
      identifier_(x) |>
        map(
          function(i)
            request(i) |>
            req_throttle(capacity = 30, fill_time_s = 60) |>
            req_url_query(splice(query)) |>
            req_error(is_error = ~ FALSE)
        )
    )
  }

  yrs_valid <- years %in_% years_(x)

  if (!all(yrs_valid)) {
    cli_abort(
      c(" " = "{metadata_(x)$title}",
        "x" = paste0("Invalid {.arg years}: ",
        "{.val {years[which_(yrs_valid, TRUE)]}}."),
        ">" = "Valid: {.pkg {paste0(range(years_(x)), collapse = '-')}}"
        ),
      call = caller_env())
  }

  urls <- name_years_(identifier_(x), x)[as.character(years)]

  flist(
    years = names(urls) |> as.integer(),
    requests = map(urls, function(i) request(i) |>
        req_throttle(capacity = 30, fill_time_s = 60) |>
          req_url_query(splice(query)) |>
          req_error(is_error = ~ FALSE)
    )
  )
}

method(base_request, class_group) <- function(x, query = NULL, years = NULL) {
  map2(
    members_(x),
    list2(query = query, years = years),
    function(m, q) {
      base_request(m, splice(q))
    })
}

#' @name query_nresults
#' @title Request the number of results for a query
#' @param x An `<int>` vector
#' @param ... Additional arguments
#' @returns An `<int>` vector
#' @autoglobal
#' @export
query_nresults <- new_generic("query_nresults", "x")

method(query_nresults, class_endpoint) <- function(x) {

  if (clog_(x) != "care") {
    return(
      base_request(x) |>
        perform_simple() |>
        _[["count"]]
      )
  }

  base_request(x) |>
    req_url_path_append("stats") |>
    perform_simple() |>
    get_elem("found_rows")
}

method(query_nresults, class_temporal) <- function(x) {

  if (clog_(x) != "care") {
    return(
      base_request(x) |>
        req_perform_parallel(on_error = "continue") |>
        map(function(e) resp_simple_json(e) |> _[["count"]]) |>
        unlist(use.names = FALSE) |>
        name_years_(x)
    )
  }
  base_request(x) |>
    map(function(i) req_url_path_append(i, "stats")) |>
    req_perform_parallel(on_error = "continue") |>
    map(function(e) get_elem(resp_simple_json(e), "found_rows")) |>
    unlist(use.names = FALSE) |>
    name_years_(x)
}

method(query_nresults, class_group) <- function(x) {
  members_(x) |>
    map(\(x) query_nresults(x), .progress = TRUE) |>
    name_members_(x)
}
