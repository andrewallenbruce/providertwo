#' @autoglobal
#' @noRd
quick_ <- new_generic("quick_", "x", function(x, ..., offset, limit) {
  S7_dispatch()
})

method(quick_, careMain) <- function(x, offset, limit) {
  x <- prop(x, "identifier") |>
    request() |>
    req_url_query(
      offset = thresh(offset, total_rows(x)),
      size   = thresh(limit, 5000L)
      ) |>
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
      req_url_query(
        offset = thresh(offset, total_rows(x)),
        size   = thresh(limit, 5000L)
        )
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
      offset  = thresh(offset, total_rows(x)),
      limit   = thresh(limit, 2000L)
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
          req_url_query(
            count   = "false",
            format  = "json",
            keys    = "true",
            results = "true",
            rowIds  = "false",
            schema  = "false",
            offset  = thresh(offset, total_rows(x)),
            limit   = thresh(limit, 2000L)
            )) |>
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

method(quick_, openMain) <- function(x, offset, limit) {
  prop(x, "identifier") |>
    request() |>
    req_url_query(
      count   = "false",
      format  = "json",
      keys    = "true",
      results = "true",
      rowIds  = "false",
      schema  = "false",
      offset  = thresh(offset, total_rows(x)),
      limit   = thresh(limit, 500L)
    ) |>
    perform_simple() |>
    _[["results"]] |>
    map_na_if() |>
    rnm(clean_names) |>
    as_tbl()
}

method(quick_, openGroup) <- function(x, offset, limit) {

  prop(x, "members") |>
    map(\(x)
        prop(x, "identifier") |>
          request() |>
          req_url_query(
            count   = "false",
            format  = "json",
            keys    = "true",
            results = "true",
            rowIds  = "false",
            schema  = "false",
            offset  = thresh(offset, total_rows(x)),
            limit   = thresh(limit, 500L)
          )) |>
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
      offset  = thresh(offset, total_rows(x)),
      limit   = thresh(limit, 8000L)
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
          req_url_query(
            count   = "false",
            format  = "json",
            keys    = "true",
            results = "true",
            rowIds  = "false",
            schema  = "false",
            offset  = thresh(offset, total_rows(x)),
            limit   = thresh(limit, 8000L)
            )) |>
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
    contact           = ,
    crosswalk         = ,
    dialysis          = ,
    enrollees         = ,
    facilities        = ,
    IQIES             = ,
    laboratories      = ,
    long_term         = ,
    opt_out           = ,
    order_refer       = ,
    RBCS              = ,
    transparency      = quick_(careMain(x), offset = offset, limit = limit),
    HHA               = ,
    hospice           = ,
    hospital          = ,
    RHC               = ,
    FQHC              = ,
    pending           = ,
    reassignment      = ,
    SNF               = quick_(careGroup(x), offset = offset, limit = limit),
    suppliers         = quick_(proMain(x), offset = offset, limit = limit),
    PDC               = ,
    MIPS              = ,
    LTCH              = ,
    IRF               = ,
    SPICE             = ,
    CAHPS_hospice     = ,
    HHVBP             = ,
    HHC               = ,
    HHCAHPS           = quick_(proGroup(x), offset = offset, limit = limit),
    PROF_covered      = ,
    PROF_physician    = ,
    PROF_information  = ,
    PROF_mapping      = ,
    PROF_entity       = ,
    PROF_teaching     = ,
    SUMM_dashboard    = ,
    SUMM_state_all    = ,
    SUMM_state_group  = ,
    SUMM_nation_all   = ,
    SUMM_nation_group = quick_(openMain(x), offset = offset, limit = limit),
    profile           = ,
    summary           = quick_(openGroup(x), offset = offset, limit = limit),
    MLR               = ,
    enterprise        = quick_(caidMain(x), offset = offset, limit = limit),
    demographics      = quick_(caidGroup(x), offset = offset, limit = limit),

    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
}
