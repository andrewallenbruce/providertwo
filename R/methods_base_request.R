#' @include S7_classes.R
NULL

#' @autoglobal
#' @noRd
default_query <- new_generic("default_query", "x")

method(default_query, class_backend) <- function(x) {
  switch(
    clog_(x),
    care = list(offset = 0L, size = 1L),
    caid = ,
    hgov = ,
    open = ,
    prov = list(count = "true", results = "true", offset = 0L, limit = 1L)
  )
}

#' @name base_request
#' @title Create a new `<httr2_request>` by class
#' @param x A `class_endpoint`, `class_temporal` or `class_group` object
#' @param ... Additional arguments
#' @returns An `<httr2_request>` object or list of `<httr2_request>` objects
#' @examples
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

method(base_request, class_endpoint) <- function(x) {
  identifier_(x) |>
    request() |>
    req_throttle(capacity = 30, fill_time_s = 60) |>
    req_url_query(splice(default_query(x))) |>
    req_error(is_error = ~ FALSE)
}

method(base_request, class_temporal) <- function(x) {
  identifier_(x) |>
    map(
      function(i)
        request(i) |>
        req_throttle(capacity = 30, fill_time_s = 60) |>
        req_url_query(splice(default_query(x))) |>
        req_error(is_error = ~ FALSE)
    )
}

method(base_request, class_group) <- function(x) {
  members_(x) |> map(base_request)
}
