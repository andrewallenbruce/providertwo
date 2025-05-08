#' @include S7_care.R
#' @include S7_pro.R
#' @include S7_open.R
#' @include S7_caid.R
NULL

#' @name base_request
#' @title Create a new request by class
#' @param x An object of class `care_endpoint`, `care_group`, `pro_endpoint`, or `open_endpoint`
#' @returns A new base request
#' @examples
#' care_endpoint("care_enrollees") |> base_request()
#' pro_endpoint("pdc_affiliations") |> base_request()
#' open_endpoint("profile_covered") |> base_request()
#' caid_endpoint("mlr_summary") |> base_request()
#'
#' care_group("care_hospital") |> base_request()
#' pro_group("pro_mips") |> base_request()
#'
#' # care_temporal("quality_payment") |> base_request()
#' # caid_temporal("healthcare_quality") |> base_request()
#' # open_temporal("payment_general") |> base_request()
#'
#' # care_troup("care_utilization") |> base_request()
#' # open_troup("payment_grouped") |> base_request()
#' @autoglobal
#' @export
base_request <- new_generic("base_request", "x", function(x) {
  S7_dispatch()
})

method(base_request, class_character) <- function(x) {
  x |>
    request() |>
    req_throttle(capacity = 30, fill_time_s = 60) |>
    req_url_query(offset = 0L)
}

## ----------(endpoint)
method(base_request, care_endpoint) <- function(x) {
  x@identifier |>
    base_request() |>
    req_url_query(size = 5000L)
}

method(base_request, caid_endpoint) <- function(x) {
  x@identifier |>
    base_request() |>
    req_url_query(
      count   = "false",
      format  = "json",
      keys    = "true",
      results = "true",
      rowIds  = "false",
      schema  = "false",
      limit   = 8000L
    )
}

method(base_request, pro_endpoint) <- function(x) {
  x@identifier |>
    base_request() |>
    req_url_query(
      count   = "false",
      format  = "json",
      keys    = "true",
      results = "true",
      rowIds  = "false",
      schema  = "false",
      limit   = 2000L
    )
}

method(base_request, open_endpoint) <- function(x) {
  x@identifier |>
    base_request() |>
    req_url_query(
      count   = "false",
      format  = "json",
      keys    = "true",
      results = "true",
      rowIds  = "false",
      schema  = "false",
      limit   = 500L
    )
}

## ----------(temporal)
method(base_request, care_temporal) <- function(x) {
  map(x@endpoints$identifier,
      \(x)
      base_request(x) |>
        req_url_query(size = 5000L))
}

method(base_request, caid_temporal) <- function(x) {
  map(
    x@endpoints$identifier,
    \(x)
    base_request(x) |>
      req_url_query(
        count   = "false",
        format  = "json",
        keys    = "true",
        results = "true",
        rowIds  = "false",
        schema  = "false",
        limit   = 8000L
      )
  )
}

method(base_request, open_temporal) <- function(x) {
  map(
    x@endpoints$identifier,
    \(x)
    base_request(x) |>
      req_url_query(
        count   = "false",
        format  = "json",
        keys    = "true",
        results = "true",
        rowIds  = "false",
        schema  = "false",
        limit   = 500L
      )
  )
}

## ----------(group)
method(base_request, care_group) <- function(x) { map(x@members, base_request) }
method(base_request, open_group) <- function(x) { map(x@members, base_request) }
method(base_request, pro_group)  <- function(x) { map(x@members, base_request) }
method(base_request, caid_group) <- function(x) { map(x@members, base_request) }

## ----------(troup)
method(base_request, care_troup) <- function(x) { map(x@members, base_request) }
method(base_request, open_troup) <- function(x) { map(x@members, base_request) }
