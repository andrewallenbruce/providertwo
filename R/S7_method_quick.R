#' @include S7_care.R
#' @include S7_pro.R
#' @include S7_open.R
#' @include S7_caid.R

#' @autoglobal
#' @noRd
quick_ <- new_generic("quick_", "x", function(x, ..., offset, limit) {
  S7_dispatch()
})

method(quick_, careMain) <- function(x, offset, limit) {
  x <- identifier(x) |>
    request() |>
    req_url_query(offset = thresh(offset, rows(x)),
                  size   = thresh(limit, 5000L)) |>
    perform_simple()

  x$data |>
    as_tbl() |>
    set_clean(x$meta$headers) |>
    map_na_if()
}

method(quick_, careGroup) <- function(x, offset, limit) {
  o <- members(x) |>
    map(\(x) identifier(x) |>
          request() |>
          req_url_query(
            offset = thresh(offset, rows(x)),
            size   = thresh(limit, 5000L)
          )) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes()

  nms <- map(o, \(resp) resp_simple_json(resp) |> _[["meta"]] |> _[["headers"]])

  map2(
    o,
    nms,
    \(resp, nm)
    resp_body_string(resp) |>
      fparse(query = "/data") |>
      as_tbl() |>
      set_clean(nm) |>
      map_na_if()
  ) |>
    set_names(members_names(x))
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
    map_na_if() |>
    rnm(clean_names) |>
    as_tbl()
}

method(quick_, pro_group) <- function(x, offset, limit) {
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
          limit   = thresh(limit, 2000L)
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
    set_names(members_names(x))
}

method(quick_, openMain) <- function(x, offset, limit) {
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

method(quick_, openGroup) <- function(x, offset, limit) {
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
          limit   = thresh(limit, 500L)
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
    set_names(members_names(x))
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
    set_names(members_names(x))
}

# quick("MLR")
# quick("enterprise")
# quick("demographics")
#
# quick("suppliers")
# quick("PDC")
# quick("MIPS")
# quick("LTCH")
# quick("IRF")
# quick("SPICE")
# quick("CAHPS_hospice")
# quick("HHVBP")
# quick("HHCA")
# quick("HHCAHPS")
# quick("SNF_VBP")
# quick("SNF_quality")
# quick("NH_pro")
#
# quick("summary")
# quick("profile")
#
# quick("HHA")
# quick("hospice")
# quick("hospital")
# quick("RHC")
# quick("FQHC")
# quick("pending")
# quick("reassignment")
# quick("SNF")
#
# quick("contact")
# quick("crosswalk")
# quick("CARE_dialysis")
# quick("enrollees")
# quick("facilities")
# quick("IQIES")
# quick("laboratories")
# quick("long_term")
# quick("opt_out")
# quick("order_refer")
# quick("RBCS")
# quick("transparency")
