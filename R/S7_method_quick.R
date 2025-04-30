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
  identifier(x) |>
    request() |>
    req_url_query(offset = thresh(offset, rows(x)),
                  size   = thresh(limit, 5000L)) |>
    perform_simple() |>
    _[["data"]] |>
    as_tbl() |>
    set_clean(fields(x)) |>
    map_na_if()
}

method(quick_, care_temporal) <- function(x, offset, limit) {
  map2(
    endpoints(x)$identifier,
    rows(x),
    \(x, y)
    request(x) |>
      req_url_query(
        offset = thresh(offset, y),
        size   = thresh(limit, 5000L))) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    map2(
      endpoints(x)$year,
      \(resp, yr)
      resp_body_string(resp) |>
        fparse() |>
        mtt(year = yr) |>
        colorder(year, npi)
    ) |>
    rowbind(fill = TRUE) |>
    map_na_if() |>
    as_tbl()
}

method(quick_, care_group) <- function(x, offset, limit) {
  map(
    members(x),
    \(x) identifier(x) |>
      request() |>
      req_url_query(
        offset = thresh(offset, rows(x)),
        size   = thresh(limit, 5000L)
      )
  ) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    map2(
      members(x),
      \(resp, n) resp_body_string(resp) |>
        fparse(query = "/data") |>
        as_tbl() |>
        set_clean(fields(n)) |>
        map_na_if()
    ) |>
    set_members_names(x)
}

method(quick_, pro_endpoint) <- function(x, offset, limit) {
  identifier(x) |>
    request() |>
    req_url_query(
      count   = "false",
      format  = "json",
      keys    = "true",
      results = "true",
      rowIds  = "false",
      schema  = "false",
      offset  = thresh(offset, rows(x)),
      limit   = thresh(limit, 2000L)
    ) |>
    perform_simple() |>
    _[["results"]] |>
    as_tbl() |>
    set_clean(fields(x)) |>
    map_na_if()
}

method(quick_, pro_group) <- function(x, offset, limit) {
  members(x) |>
    map(
      \(x) identifier(x) |>
        request() |>
        req_url_query(
          count   = "false",
          format  = "json",
          keys    = "true",
          results = "true",
          rowIds  = "false",
          schema  = "false",
          offset  = thresh(offset, rows(x)),
          limit   = thresh(limit, 2000L)
        )
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
    set_members_names(x)
}

method(quick_, open_endpoint) <- function(x, offset, limit) {
  identifier(x) |>
    request() |>
    req_url_query(
      count   = "false",
      format  = "json",
      keys    = "true",
      results = "true",
      rowIds  = "false",
      schema  = "false",
      offset  = thresh(offset, rows(x)),
      limit   = thresh(limit, 500L)
    ) |>
    perform_simple() |>
    _[["results"]] |>
    map_na_if() |>
    rnm(clean_names) |>
    as_tbl()
}

method(quick_, open_group) <- function(x, offset, limit) {
  members(x) |>
    map(
      \(x) identifier(x) |>
        request() |>
        req_url_query(
          count   = "false",
          format  = "json",
          keys    = "true",
          results = "true",
          rowIds  = "false",
          schema  = "false",
          offset  = thresh(offset, rows(x)),
          limit   = thresh(limit, 500L)
        )
    ) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    map(
      \(resp) resp_body_string(resp) |>
        fparse() |>
        _[["results"]] |>
        as_tbl() |>
        map_na_if()
      ) |>
    set_members_names(x)
}

method(quick_, caid_endpoint) <- function(x, offset, limit) {
  identifier(x) |>
    request() |>
    req_url_query(
      count   = "false",
      format  = "json",
      keys    = "true",
      results = "true",
      rowIds  = "false",
      schema  = "false",
      offset  = thresh(offset, rows(x)),
      limit   = thresh(limit, 8000L)
    ) |>
    perform_simple() |>
    _[["results"]] |>
    map_na_if() |>
    rnm(clean_names) |>
    as_tbl()
}

method(quick_, caid_group) <- function(x, offset, limit) {
  members(x) |>
    map(
      \(x)
      identifier(x) |>
        request() |>
        req_url_query(
          count   = "false",
          format  = "json",
          keys    = "true",
          results = "true",
          rowIds  = "false",
          schema  = "false",
          offset  = thresh(offset, rows(x)),
          limit   = thresh(limit, 8000L)
        )
    ) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    map(\(resp)
        resp_body_string(resp) |>
          fparse() |>
          _[["results"]] |>
          as_tbl() |>
          map_na_if()) |>
    set_members_names(x)
}
