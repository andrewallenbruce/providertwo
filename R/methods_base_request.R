#' @include S7_classes.R
NULL

#' @autoglobal
#' @noRd
default_query <- new_generic("default_query", "x")

method(default_query, class_backend) <- function(x) {

  if (clog_(x) != "care")
    return(list2(
      count   = "true",
      results = "false",
      offset  = 0L,
      limit   = 1L
    ))

  list2(offset = 0L, size = 1L)
}

#' @name base_request
#' @title Create a new `<httr2_request>` by class
#' @param x A `class_endpoint`, `class_temporal` or `class_group` object
#' @param ... Additional arguments
#' @returns An `<httr2_request>` object or list of `<httr2_request>` objects
#' @examplesIf interactive()
#' care_endpoint("care_enrollees") |> base_request()
#' care_temporal("quality_payment") |> base_request()
#' care_group("care_hospital") |> base_request()
#' prov_endpoint("pdc_affiliations") |> base_request()
#' prov_group("pro_mips") |> base_request()
#' open_endpoint("profile_covered") |> base_request()
#' open_temporal("payment_general") |> base_request()
#' open_group("payment_grouped") |> base_request()
#' caid_endpoint("mlr_summary") |> base_request()
#' caid_temporal("healthcare_quality") |> base_request()
#' hgov_endpoint("hgov_catastrophic") |> base_request()
#' hgov_temporal("hgov_mlr") |> base_request()
#' @autoglobal
#' @export
base_request <- new_generic("base_request", "x")

method(base_request, class_endpoint) <- function(x, query = NULL, years = NULL) {
  identifier_(x) |>
    request() |>
    req_throttle(capacity = 30, fill_time_s = 60) |>
    req_url_query(splice(default_query(x)), splice(query)) |>
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
            req_url_query(splice(default_query(x)), splice(query)) |>
            req_error(is_error = ~ FALSE)
        )
    )
  }

  if (all(years %!in_% years_(x))) {
    cli_abort(
      c(" " = "{metadata_(x)$title}",
        "x" = paste0("Invalid {.arg years}: ",
        "{.val {years[which_(years %!in_% years_(x))]}}."),
        ">" = "Valid: {.pkg {paste0(range(years_(x)), collapse = '-')}}"
        ),
      call = caller_env())
  }

  list_tidy(
    urls_years = set_names(identifier_(x), years_(x))[as.character(years)],
    requests   = map(urls_years, function(i) request(i) |>
        req_throttle(capacity = 30, fill_time_s = 60) |>
        req_url_query(splice(default_query(x), splice(query))) |>
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
#' @examples
#' care_endpoint("care_enrollees") |> query_nresults()
#' care_temporal("quality_payment") |> query_nresults()
#' care_group("care_hospital") |> query_nresults()
#' prov_endpoint("pdc_affiliations") |> query_nresults()
#' prov_group("pro_mips") |> query_nresults()
#' open_endpoint("profile_covered") |> query_nresults()
#' open_temporal("payment_general") |> query_nresults()
#' open_group("payment_grouped") |> query_nresults()
#' caid_endpoint("mlr_summary") |> query_nresults()
#' caid_temporal("healthcare_quality") |> query_nresults()
#' hgov_endpoint("hgov_catastrophic") |> query_nresults()
#' hgov_temporal("hgov_mlr") |> query_nresults()
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
    map(query_nresults, .progress = TRUE) |>
    name_members_(x)
}
