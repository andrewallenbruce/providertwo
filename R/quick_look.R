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


# quick("mlr")
# quick("mesd")
# quick("wcv")
# quick("mhsud")
# quick("disability")
# quick("livebirth")
# quick("lang")
# quick("race")
# quick("rural")
# quick("waive")
#
# quick("PDC")
# quick("MIPS")
#
# quick("affiliations")
# quick("clinicians")
# quick("utilization")
# quick("MIPS_perform")
# quick("MIPS_patient")
# quick("MIPS_clinician")
# quick("MIPS_group")
# quick("MIPS_virtual")
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
    transparency   = quick_(careMain(x), offset = offset, limit = cheapr_if_else(limit > 5000L, 5000L, limit)),
    HHA            = ,
    hospice        = ,
    hospital       = ,
    RHC            = ,
    FQHC           = ,
    pending        = ,
    reassignment   = ,
    SNF            = quick_(careGroup(x), offset = offset, limit = cheapr_if_else(limit > 5000L, 5000L, limit)),
    affiliations   = ,
    clinicians     = ,
    utilization    = ,
    MIPS_perform   = ,
    MIPS_patient   = ,
    MIPS_clinician = ,
    MIPS_group     = ,
    MIPS_virtual   = quick_(proMain(x), offset = offset, limit = cheapr_if_else(limit > 2000L, 2000L, limit)),
    MIPS           = ,
    PDC            = quick_(proGroup(x), offset = offset, limit = cheapr_if_else(limit > 2000L, 2000L, limit)),
    mlr            = ,
    mesd           = ,
    wcv            = ,
    mhsud          = ,
    disability     = ,
    pi             = ,
    livebirth      = ,
    blood          = ,
    lang           = ,
    race           = ,
    rural          = ,
    waive          = ,
    newdrug_01     = ,
    newdrug_02     = ,
    newdrug_03     = ,
    newdrug_04     = ,
    newdrug_05     = ,
    newdrug_06     = ,
    newdrug_07     = ,
    newdrug_08     = ,
    newdrug_09     = ,
    newdrug_10     = ,
    newdrug_11     = ,
    newdrug_12     = ,
    newdrug_13     = ,
    newdrug_14     = ,
    newdrug_15     = ,
    newdrug_16     = quick_(caidMain(x), offset = offset, limit = cheapr_if_else(limit > 8000L, 8000L, limit)),

    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
}

# quick_care("enrollees")
#' @autoglobal
#' @noRd
quick_care <- function(x, offset = 0L, limit = 5000L) {
  x <- careMain(alias = x) |>
    prop("identifier") |>
    request() |>
    req_url_query(offset = offset, size = limit) |>
    perform_simple()

  x$data |>
    as_tbl() |>
    set_clean(x$meta$headers) |>
    map_na_if()
}

# quick_care_group("RHC")
#' @autoglobal
#' @noRd
quick_care_group <- function(x, offset = 0L, limit = 5000L) {

  obj <- careGroup(alias = x) |> prop("members")

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

# quick_pro("clinicians")
#' @autoglobal
#' @noRd
quick_pro <- function(x, offset = 0L, limit = 2000L) {
  proMain(alias = x) |>
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

quick_caid <- function(x, offset = 0L, limit = 8000L) {
  x |>
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
