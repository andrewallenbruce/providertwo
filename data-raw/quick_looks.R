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
