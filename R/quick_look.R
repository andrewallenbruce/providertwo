#' @autoglobal
#' @noRd
quick_ <- new_generic("quick", "x", function(x, ..., offset, limit) {
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

# quick("enrollees")
# quick("RHC")
#' @autoglobal
#' @noRd
quick <- function(x, offset = 0L, limit = 5000L, call = caller_env()) {
  switch(
    x,
    enrollees = quick_(careMain("enrollees"), offset = offset, limit = limit),
    RHC = quick_(careGroup("RHC"), offset = offset, limit = limit),
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
