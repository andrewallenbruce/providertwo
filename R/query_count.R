#' @autoglobal
#' @noRd
query_count <- new_generic("query_count", "obj")

method(query_count, class_group) <- function(obj) {
  prop(obj, "members") |>
    map(query_count)
}

method(query_count, class_care) <- function(obj) {
  prop(obj, "identifier") |>
    query_count()
}

method(query_count, class_current) <- function(obj, query = NULL) {
  S7_data(obj) |>
    map(request) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    map(function(resp)
      parse_string(resp, query = "/data") |>
        tidy_resources()) |>
    yank()
}

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
