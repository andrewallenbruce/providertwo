#' Return the number of results for a given query
#'
#' @param obj An object of class `<class_care>`, `<class_group>`, or `<class_collection>`.
#'
#' @param ... Additional arguments passed to the query.
#'
#' @returns An `<int>` representing the number of results found.
#'
#' @examples
#' endpoint("care_dial_end") |>
#' query_count(query(
#' state = in_(c("CA", "GA", "NY"),
#' care = TRUE),
#' .type = "medicare"))
#'
#' collection("care_rhc") |>
#' query_count(
#' query(STATE = in_(c("CA", "GA", "NY"), care = TRUE),
#'       `STATE - OWNER` = in_(c("CA", "GA", "NY"), care = TRUE),
#'       .type = "medicare"))
#'
#' @autoglobal
#' @export
query_count <- new_generic("query_count", "obj")

method(query_count, class_group) <- function(obj, query = NULL) {
  prop(obj, "members") |>
    walk(\(x) query_count(x, query = query))
}

method(query_count, class_care) <- function(obj, query = NULL) {
  x <- prop(obj, "identifier") |>
    S7_data() |>
    httr2::url_modify(query = query@string) |>
    request() |>
    httr2::req_url_path_append("stats") |>
    perform_simple() |>
    _$data

  cli::cli_inform(
    c(
      "!" = "Results: {.val {x$found_rows}} of {.val {x$total_rows}} ({.val {roundup(x$found_rows / x$total_rows)}} %)",
      "*" = "Query: {.val {query@input}}",
      "*" = "{.cls class_care} Endpoint {.val {obj@metadata$title}}"
    )
  )
  cli::cat_line()
  invisible(x)
}

# query(STATE = in_(c("CA", "GA", "NY"), care = TRUE),
#       `STATE - OWNER` = in_(c("CA", "GA", "NY"), care = TRUE),
#       .type = "medicare")
