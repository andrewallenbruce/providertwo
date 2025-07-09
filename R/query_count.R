#' Return the number of results for a given query
#' @param obj An object of class `<class_care>`, `<class_group>`, or `<class_collection>`.
#' @param query An optional `<class_query>` object to filter the results.
#' @returns An `<int>` representing the number of results found.
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
#' @autoglobal
#' @noRd
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

# query(
#   first_name = starts_with_("Andr"),
#   last_name  = contains_("J"),
#   state      = in_(c("CA", "GA", "NY")),
#   city       = equals_(c("Atlanta", "Los Angeles"), negate = TRUE),
#   state_own  = c("GA", "MD"),
#   npi        = npi_ex$k,
#   ccn        = "01256",
#   rate       = between_(0.45, 0.67))
#
# obj <- endpoint("care_dial_end")
#
# paste0(
#   S7_data(prop(obj, "identifier")),
#   "&",
#   query(state = in_(c("CA", "GA", "NY"), care = TRUE), .type = "medicare")@string) |>
#   request() |>
#   httr2::req_url_path_append("stats") |>
#   perform_simple()

# e <- endpoint("pdc_clinicians")
# endpoint("lab_fee_sched")@identifier |> S7_data()
# q <- query(state = in_(state.abb[10:15], negate = TRUE))@string
#
# S7_data(e@identifier) |>
#   httr2::url_modify_query(limit = e@dimensions@limit) |>
#   paste0("&", q@string) |>
#   request() |>
#   perform_simple() |>
#   _$count |>
#   as_fibble()
#
# method(query_count, class_current) <- function(obj, query = NULL) {
#   S7_data(obj) |>
#     map(request) |>
#     req_perform_parallel(on_error = "continue") |>
#     resps_successes() |>
#     map(function(resp)
#       parse_string(resp, query = "/data") |>
#         tidy_resources()) |>
#     yank()
# }

# query <- "filter[0][path]=hcpcs_cd&
# filter[0][operator]==&
# filter[0][value]=80047&
# filter[1][path]=VOL_TXT&
# filter[1][operator]=<&
# filter[1][value]=207"
#
# obj <- endpoint("lab_fee_sched")
#
# prop(obj, "identifier") |>
#   S7_data() |>
#   paste0("&", query, collapse = "&") |>
#   request() |>
#   req_url_path_append("stats") |>
#   perform_simple() |>
#   _$data
#
# paste0(
#   "https://data.cms.gov/",
#   "data-api/v1/dataset/",
#   "0e57f57d-0acc-4c9c-8f8c-973e3f4a3c4b/",
#   # "data-viewer?",
#   "data-viewer/stats?",
#   paste(
#     "offset=0",
#     "size=200",
#
#     # "filter[2][group][conjunction]=AND",  # found 15
#     # "filter[g1][group][conjunction]=OR", # found 145239
#     # "filter[0][condition][memberOf]=2",
#     # "filter[1][condition][memberOf]=2",
#
#     "filter[0][condition][path]=hcpcs_cd",
#     "filter[0][condition][operator]==",
#     "filter[0][condition][value]=80047",
#
#     "filter[1][condition][path]=VOL_TXT",
#     "filter[1][condition][operator]=<",
#     "filter[1][condition][value]=207",
#
#     sep = "&"
#   )) |>
#   request() |>
#   perform_simple() |>
#   _$data
