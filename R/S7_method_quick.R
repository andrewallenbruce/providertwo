#' @include S7_care.R
#' @include S7_pro.R
#' @include S7_open.R
#' @include S7_caid.R
#' @autoglobal
#' @noRd
quick_ <- new_generic("quick_", "x", function(x, ..., offset, limit) {
  S7_dispatch()
})

method(quick_, care_endpoint) <- function(x, offset, limit) {
  x@identifier |>
    request() |>
    req_url_query(offset = thresh(offset, x@dimensions@rows),
                  size   = thresh(limit, 5000L)) |>
    perform_simple() |>
    _[["data"]] |>
    as_tbl() |>
    set_clean(x@dimensions@fields) |>
    map_na_if()
}

method(quick_, care_temporal) <- function(x, offset, limit) {
  x@endpoints$identifier |>
    map2(x@dimensions@rows, \(x, y)
    request(x) |>
      req_url_query(
        offset = thresh(offset, y),
        size   = thresh(limit, 5000L))) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    map2(x@endpoints$year, \(resp, yr)
      resp_body_string(resp) |>
        fparse() |>
        mtt(year = yr) |>
        colorder(year, npi)
    ) |>
    rowbind(fill = TRUE) |>
    map_na_if() |>
    rnm(clean_names) |>
    as_tbl()
}

method(quick_, care_group) <- function(x, offset, limit) {
  x@members |>
    map(\(x) x@identifier |>
          request() |>
          req_url_query(
            offset = thresh(offset, x@dimensions@rows),
            size   = thresh(limit, 5000L)
            )) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    map2(
      x@members,
      \(resp, n) resp_body_string(resp) |>
        fparse(query = "/data") |>
        as_tbl() |>
        set_clean(n@dimensions@fields) |>
        map_na_if()
    ) |>
    set_names(names(x@members))
}

method(quick_, pro_endpoint) <- function(x, offset, limit) {
  x@identifier |>
    request() |>
    req_url_query(
      count   = "false",
      format  = "json",
      keys    = "true",
      results = "true",
      rowIds  = "false",
      schema  = "false",
      offset = thresh(offset, x@dimensions@rows),
      limit  = thresh(limit, 2000L)) |>
    perform_simple() |>
    _[["results"]] |>
    as_tbl() |>
    set_clean(x@dimensions@fields) |>
    map_na_if()
}

method(quick_, pro_group) <- function(x, offset, limit) {
  x@members |>
    map(
      \(x)
      x@identifier |>
        request() |>
        req_url_query(
          count   = "false",
          format  = "json",
          keys    = "true",
          results = "true",
          rowIds  = "false",
          schema  = "false",
          offset = thresh(offset, x@dimensions@rows),
          limit  = thresh(limit, 2000L))
          ) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    map(
      \(resp) resp_body_string(resp) |>
        fparse() |>
        _[["results"]] |>
        as_tbl() |>
        rnm(clean_names) |>
        map_na_if()
      ) |>
    set_names(names(x@members))
}

method(quick_, open_endpoint) <- function(x, offset, limit) {
  x@identifier |>
    request() |>
    req_url_query(
      count   = "false",
      format  = "json",
      keys    = "true",
      results = "true",
      rowIds  = "false",
      schema  = "false",
      offset = thresh(offset, x@dimensions@rows),
      limit  = thresh(limit, 500L)) |>
    perform_simple() |>
    _[["results"]] |>
    as_tbl() |>
    set_clean(x@dimensions@fields) |>
    map_na_if()
}

method(quick_, open_group) <- function(x, offset, limit) {
  x@members |>
    map(
      \(x)
      x@identifier |>
        request() |>
        req_url_query(
          count   = "false",
          format  = "json",
          keys    = "true",
          results = "true",
          rowIds  = "false",
          schema  = "false",
          offset = thresh(offset, x@dimensions@rows),
          limit  = thresh(limit, 500L))
    ) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    map(
      \(resp) resp_body_string(resp) |>
        fparse() |>
        _[["results"]] |>
        as_tbl() |>
        rnm(clean_names) |>
        map_na_if()
    ) |>
    set_names(names(x@members))
}

method(quick_, caid_endpoint) <- function(x, offset, limit) {
  x@identifier |>
    request() |>
    req_url_query(
      count   = "false",
      format  = "json",
      keys    = "true",
      results = "true",
      rowIds  = "false",
      schema  = "false",
      offset = thresh(offset, x@dimensions@rows),
      limit  = thresh(limit, 8000L)) |>
    perform_simple() |>
    _[["results"]] |>
    as_tbl() |>
    set_clean(x@dimensions@fields) |>
    map_na_if()
}

method(quick_, caid_group) <- function(x, offset, limit) {
  x@members |>
    map(
      \(x)
      x@identifier |>
        request() |>
        req_url_query(
          count   = "false",
          format  = "json",
          keys    = "true",
          results = "true",
          rowIds  = "false",
          schema  = "false",
          offset = thresh(offset, x@dimensions@rows),
          limit  = thresh(limit, 8000L))
    ) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    map(
      \(resp) resp_body_string(resp) |>
        fparse() |>
        _[["results"]] |>
        as_tbl() |>
        rnm(clean_names) |>
        map_na_if()
    ) |>
    set_names(names(x@members))
}
