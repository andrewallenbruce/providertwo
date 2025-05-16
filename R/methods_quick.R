#' @autoglobal
#' @noRd
dimensions <- function(obj) {
  check_is_S7(obj, class_endpoint)
  prop(obj, "dimensions") |>
    props(names = c("limit", "rows"))
}

#' @autoglobal
#' @noRd
fields <- function(obj) {
  check_is_S7(obj, class_endpoint)
  prop(obj, "dimensions") |>
    prop("fields") |>
    unlist(use.names = FALSE)
}

#' @autoglobal
#' @noRd
identifier <- function(obj) {
  check_is_S7(obj, class_endpoint)
  list_combine(prop(obj, "identifier"))
}

#' @autoglobal
#' @noRd
members <- function(obj) {
  check_is_S7(obj, class_group)
  prop(obj, "members")
}

#' @autoglobal
#' @noRd
name_members <- function(x, obj) {
  set_names(x, names(members(obj)))
}

#' @autoglobal
#' @noRd
name_fields <- function(x, obj) {
  set_names(x, fields(obj))
}

#' @include S7_care.R
#' @include S7_pro.R
#' @include S7_open.R
#' @include S7_caid.R
#' @include S7_hgov.R
#' @autoglobal
#' @noRd
quick_ <- new_generic("quick_", "x", function(x, ..., offset, limit) {
  S7_dispatch()
})

method(quick_, class_endpoint) <- function(x, offset, limit) {
  identifier(x) |>
    map(
      function(i)
        request(i) |>
        req_url_query(
          count   = "false",
          format  = "json",
          keys    = "true",
          results = "true",
          rowIds  = "false",
          schema  = "false",
          offset  = thresh(offset, dimensions(x)$rows),
          limit   = thresh(limit, dimensions(x)$limit)
        )
    ) |>
    req_perform_parallel(on_error = "continue") |>
    map(function(x)
      resp_body_string(x) |>
        fparse() |>
        _[["results"]] |>
        as_tbl() |>
        map_na_if()) |>
    pluck(1) |>
    name_fields(x)
}

method(quick_, care_endpoint) <- function(x, offset, limit) {
  identifier(x) |>
    map(function(i)
      request(i) |>
        req_url_query(
          offset = thresh(offset, dimensions(x)$rows),
          size   = thresh(limit, dimensions(x)$limit)
        )) |>
    req_perform_parallel(on_error = "continue") |>
    map(function(x)
      resp_body_string(x) |>
        fparse(query = "/data") |>
        as_tbl() |>
        map_na_if()) |>
    pluck(1) |>
    name_fields(x)
}

method(quick_, class_group) <- function(x, offset, limit) {
  members(x) |>
    map(function(x)
      quick_(x, offset, limit), .progress = TRUE) |>
    name_members(x)
}

method(quick_, care_temporal) <- function(x, offset, limit) {

  x@endpoints$identifier |>
    map2(x@dimensions@rows,
      \(x, d)
      request(x) |>
        req_url_query(
          offset = thresh(offset, d),
          size = thresh(limit, 5000L)
          )
      ) |>
    req_perform_parallel(on_error = "continue") |>
    map2(x@endpoints$year, \(resp, yr)
         resp_body_string(resp) |>
           fparse() |>
           mtt(year = yr) |>
           colorder(year)
         ) |>
    rowbind(fill = TRUE) |>
    map_na_if() |>
    as_tbl()
}

method(quick_, open_temporal) <- function(x, offset, limit) {

  x@endpoints$identifier |>
    map2(x@dimensions@rows,
         \(x, d)
         request(x) |>
           req_url_query(
             count   = "false",
             format  = "json",
             keys    = "true",
             results = "true",
             rowIds  = "false",
             schema  = "false",
             offset = thresh(offset, d),
             limit  = thresh(limit, 500L)
             )
         ) |>
    req_perform_parallel(on_error = "continue") |>
    map2(x@endpoints$year, \(resp, yr)
         resp_body_string(resp) |>
           fparse() |>
           _[["results"]] |>
           mtt(year = yr) |>
           colorder(year)
         ) |>
    list_rbind() |>
    map_na_if() |>
    as_tbl()
}

method(quick_, caid_temporal) <- function(x, offset, limit) {

  x@endpoints$identifier |>
    map2(x@dimensions@rows,
      \(x, d)
      request(x) |>
        req_url_query(
          count   = "false",
          format  = "json",
          keys    = "true",
          results = "true",
          rowIds  = "false",
          schema  = "false",
          offset = thresh(offset, d),
          limit  = thresh(limit, 8000L)
        )
    ) |>
    req_perform_parallel(on_error = "continue") |>
    map2(
      x@endpoints$year,
      \(resp, yr)
      resp_body_string(resp) |>
        fparse() |>
        _[["results"]] |>
        mtt(year = yr) |>
        colorder(year)
    ) |>
    list_rbind() |>
    map_na_if() |>
    as_tbl() |>
    set_names(x@dimensions@fields)
}

method(quick_, hgov_temporal) <- function(x, offset, limit) {

  x@endpoints$identifier |>
    map2(x@dimensions@rows,
      \(x, d)
      request(x) |>
        req_url_query(
          count   = "false",
          format  = "json",
          keys    = "true",
          results = "true",
          rowIds  = "false",
          schema  = "false",
          offset = thresh(offset, d),
          limit  = thresh(limit, 500L)
        )
    ) |>
    req_perform_parallel(on_error = "continue") |>
    map2(
      x@endpoints$year,
      \(resp, yr)
      resp_body_string(resp) |>
        fparse() |>
        _[["results"]] |>
        mtt(year = yr) |>
        colorder(year)
    ) |>
    list_rbind() |>
    map_na_if() |>
    as_tbl() |>
    set_names(x@dimensions@fields)
}
