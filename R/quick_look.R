#' @autoglobal
#' @noRd
quick_ <- new_generic("quick_", "x", function(x, ..., offset, limit) {
  S7_dispatch()
})

method(quick_, careMain) <- function(x, offset, limit) {
  x <- prop(x, "identifier") |>
    request() |>
    req_url_query(offset = offset, size = limit) |>
    perform_simple()

  x$data |>
    as_tbl() |>
    set_clean(x$meta$headers) |>
    map_na_if()
}

method(quick_, careGroup) <- function(x, offset, limit) {
  obj <- prop(x, "members")

  x <- map(
    obj,
    \(x) prop(x, "identifier") |>
      request() |>
      req_url_query(offset = offset, size = limit)
  ) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes()

  nms <- map(x, \(resp)
             resp_simple_json(resp) |>
               _[["meta"]] |>
               _[["headers"]])

  map2(
    x,
    nms,
    \(resp, nm)
    resp_body_string(resp) |>
      fparse(query = "/data") |>
      as_tbl() |>
      set_clean(nm) |>
      map_na_if()
  ) |>
    set_names(names(obj))
}

method(quick_, proMain) <- function(x, offset, limit) {
  prop(x, "identifier") |>
    request() |>
    req_url_query(
      count   = "false",
      format  = "json",
      keys    = "true",
      results = "true",
      rowIds  = "false",
      schema  = "false",
      offset  = offset,
      limit   = limit
    ) |>
    perform_simple() |>
    _[["results"]] |>
    map_na_if() |>
    rnm(clean_names) |>
    as_tbl()
}

method(quick_, proGroup) <- function(x, offset, limit) {

  prop(x, "members") |>
    map(\(x)
        prop(x, "identifier") |>
          request() |>
          req_url_query(offset = offset,
                        size   = limit)
        ) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    map(\(resp)
        resp_body_string(resp) |>
          fparse() |>
          _[["results"]] |>
          as_tbl() |>
          map_na_if()
        ) |>
    set_names(
      prop(x, "members") |>
        names()
      )
}

method(quick_, caidMain) <- function(x, offset, limit) {
  prop(x, "identifier") |>
    request() |>
    req_url_query(
      count   = "false",
      format  = "json",
      keys    = "true",
      results = "true",
      rowIds  = "false",
      schema  = "false",
      offset  = offset,
      limit   = limit
    ) |>
    perform_simple() |>
    _[["results"]] |>
    map_na_if() |>
    rnm(clean_names) |>
    as_tbl()
}

method(quick_, caidGroup) <- function(x, offset, limit) {

  prop(x, "members") |>
    map(\(x)
        prop(x, "identifier") |>
          request() |>
          req_url_query(offset = offset,
                        size   = limit)
    ) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    map(\(resp)
        resp_body_string(resp) |>
          fparse() |>
          _[["results"]] |>
          as_tbl() |>
          map_na_if()
    ) |>
    set_names(
      prop(x, "members") |>
        names()
    )
}

# quick("MLR")
# quick("enterprise")
# quick("demographics")
#
# quick("PDC")
# quick("MIPS")
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
# quick("dialysis")
# quick("enrollees")
# quick("facilities")
# quick("IQIES")
# quick("laboratories")
# quick("long_term")
# quick("opt_out")
# quick("order_refer")
# quick("RBCS")
# quick("transparency")
#' @autoglobal
#' @noRd
quick <- function(x, offset = 0L, limit = 10000L, call = caller_env()) {
  switch(
    x,
    contact        = ,
    crosswalk      = ,
    dialysis       = ,
    enrollees      = ,
    facilities     = ,
    IQIES          = ,
    laboratories   = ,
    long_term      = ,
    opt_out        = ,
    order_refer    = ,
    RBCS           = ,
    transparency   = quick_(careMain(x), offset = offset, limit = thresh(limit, 5000L)),
    HHA            = ,
    hospice        = ,
    hospital       = ,
    RHC            = ,
    FQHC           = ,
    pending        = ,
    reassignment   = ,
    SNF            = quick_(careGroup(x), offset = offset, limit = thresh(limit, 5000L)),
    # affiliations   = ,
    # clinicians     = ,
    # utilization    = ,
    # MIPS_perform   = ,
    # MIPS_patient   = ,
    # MIPS_clinician = ,
    # MIPS_group     = ,
    # MIPS_virtual   = quick_(proMain(x), offset = offset, limit = thresh(limit, 2000L)),
    MIPS           = ,
    PDC            = quick_(proGroup(x), offset = offset, limit = thresh(limit, 2000L)),
    MLR            = ,
    enterprise     = quick_(caidMain(x), offset = offset, limit = thresh(limit, 8000L)),
    demographics   = quick_(caidGroup(x), offset = offset, limit = thresh(limit, 8000L)),

    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
}

#' @autoglobal
#' @noRd
thresh <- function(n, threshold) {
  cheapr_if_else(n > threshold, threshold, n)
}

# quick_care_temp("quality_payment")
#' @autoglobal
#' @noRd
quick_care_temp <- function(x, offset = 0L, limit = 5000L) {
  careTemp(alias = x) |>
    prop("endpoints") |>
    _[["identifier"]][1] |>
    request() |>
    req_url_query(offset = offset, size = limit) |>
    perform_simple() |>
    map_na_if() |>
    rnm(clean_names) |>
    as_tbl()
}

# quick_open("profile_covered")
#' @autoglobal
#' @noRd
quick_open <- function(x, offset = 0L, limit = 500L) {
  openMain(alias = x) |>
    prop("identifier") |>
    request() |>
    req_url_query(
      count   = "false",
      format  = "json",
      keys    = "true",
      results = "true",
      rowIds  = "false",
      schema  = "false",
      offset  = offset,
      limit   = limit
    ) |>
    perform_simple() |>
    _[["results"]] |>
    map_na_if() |>
    rnm(clean_names) |>
    as_tbl()
}

# quick_open_temp("ownership")
#' @autoglobal
#' @noRd
quick_open_temp <- function(x, offset = 0L, limit = 500L) {
  openTemp(alias = x) |>
    prop("endpoints") |>
    _[["identifier"]][1] |>
    request() |>
    req_url_query(
      count   = "false",
      format  = "json",
      keys    = "true",
      results = "true",
      rowIds  = "false",
      schema  = "false",
      offset  = offset,
      limit   = limit
    ) |>
    perform_simple() |>
    _[["results"]] |>
    map_na_if() |>
    rnm(clean_names) |>
    as_tbl()
}

