#' @include S7_classes.R
#' @include S7_care.R
NULL

#' @name base_request
#' @title Create a new `<httr2_request>` by class
#' @param x A `class_endpoint`, `class_temporal` or `class_group` object
#' @returns An `<httr2_request>` object or list of `<httr2_request>` objects
#' @examplesIf rlang::is_interactive()
#' care_endpoint("care_enrollees") |> base_request()
#' care_temporal("quality_payment") |> base_request()
#' care_group("care_hospital") |> base_request()
#' pro_endpoint("pdc_affiliations") |> base_request()
#' pro_group("pro_mips") |> base_request()
#' open_endpoint("profile_covered") |> base_request()
#' open_temporal("payment_general") |> base_request()
#' open_group("payment_grouped") |> base_request()
#' caid_endpoint("mlr_summary") |> base_request()
#' caid_temporal("healthcare_quality") |> base_request()
#' hgov_endpoint("catastrophic_plans") |> base_request()
#' hgov_temporal("medical_loss_ratio") |> base_request()
#' @autoglobal
#' @export
base_request <- new_generic("base_request", "x", function(x) {
  S7_dispatch()
})

method(base_request, class_endpoint) <- function(x) {
  identifier_(x) |>
    request() |>
    req_throttle(capacity = 30, fill_time_s = 60) |>
    req_url_query(
      count   = "false",
      format  = "json",
      keys    = "true",
      results = "true",
      rowIds  = "false",
      schema  = "false",
      offset  = 0L,
      limit   = limit_(x)
    )
}

method(base_request, care_endpoint) <- function(x) {
  identifier_(x) |>
    request() |>
    req_throttle(capacity = 30, fill_time_s = 60) |>
    req_url_query(offset = 0L, size = limit_(x))
}

method(base_request, class_temporal) <- function(x) {
  identifier_(x) |>
    map(
      function(i)
        request(i) |>
        req_throttle(capacity = 30, fill_time_s = 60) |>
        req_url_query(
          count   = "false",
          format  = "json",
          keys    = "true",
          results = "true",
          rowIds  = "false",
          schema  = "false",
          offset  = 0L,
          limit   = limit_(x)
        )
    )
}

method(base_request, care_temporal) <- function(x) {
  identifier_(x) |>
    map(
      function(i)
        request(i) |>
        req_throttle(capacity = 30, fill_time_s = 60) |>
        req_url_query(offset = 0L, limit = limit_(x))
    )
}

method(base_request, class_group) <- function(x) {
  members_(x) |> map(base_request)
}
