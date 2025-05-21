#' @include S7_care.R
#' @include S7_open.R
#' @include S7_caid.R
#' @include S7_hgov.R
NULL

#' @name base_request
#' @title Create a new request by class
#' @param x A `class_endpoint`, `class_temporal` or `class_group`
#' @returns A new base request
#' @examplesIf interactive()
#' care_endpoint("care_enrollees") |> base_request()
#' pro_endpoint("pdc_affiliations") |> base_request()
#' open_endpoint("profile_covered") |> base_request()
#' caid_endpoint("mlr_summary") |> base_request()
#' care_temporal("quality_payment") |> base_request()
#' caid_temporal("healthcare_quality") |> base_request()
#' open_temporal("payment_general") |> base_request()
#' care_group("care_hospital") |> base_request()
#' pro_group("pro_mips") |> base_request()
#' care_group("care_utilization") |> base_request()
#' open_group("payment_grouped") |> base_request()
#' @autoglobal
#' @export
base_request <- new_generic("base_request", "x", function(x) {
  S7_dispatch()
})

method(base_request, class_endpoint) <- function(x) {
  prop(x, "identifier") |>
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
      limit   = dimensions(x)$limit
    )
}

method(base_request, care_endpoint) <- function(x) {
  prop(x, "identifier") |>
    request() |>
    req_throttle(capacity = 30, fill_time_s = 60) |>
    req_url_query(offset = 0L, size = dimensions(x)$limit)
}

method(base_request, class_temporal) <- function(x) {
  prop(x, "endpoints") |>
    get_elem("identifier") |>
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
          limit   = dimensions(x)$limit
        )
    )
}

method(base_request, care_temporal) <- function(x) {
  prop(x, "endpoints") |>
    get_elem("identifier") |>
    map(
      function(i)
        request(i) |>
        req_throttle(capacity = 30, fill_time_s = 60) |>
        req_url_query(offset = 0L, limit = dimensions(x)$limit)
    )
}

method(base_request, class_group) <- function(x) {
  map(members(x), base_request)
}
