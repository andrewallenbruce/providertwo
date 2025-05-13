#' @autoglobal
#' @noRd
dimensions <- function(x) {
  props(x@dimensions, names = c("limit", "rows"))
}

#' @autoglobal
#' @noRd
fields <- function(x) {
  prop(x@dimensions, "fields") |>
    unlist(use.names = FALSE)
}

#' @autoglobal
#' @noRd
identifier <- function(x) {
  prop(x, "identifier") |> as.list()
}

#' @autoglobal
#' @noRd
set_member_names <- function(x, obj) {
  set_names(x, names(obj))
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

method(quick_, class_group) <- function(x, offset, limit) {
  x@members |>
    map(\(x) quick_(x, offset, limit), .progress = TRUE) |>
    set_member_names(x@members)
}

method(quick_, care_endpoint) <- function(x, offset, limit) {

  identifier(x) |>
    map(\(i)
        request(i) |>
          req_url_query(
            offset = thresh(offset, dimensions(x)$rows),
            size   = thresh(limit, dimensions(x)$limit))) |>
    req_perform_parallel(on_error = "continue") |>
    map(\(x) resp_body_string(x) |>
          fparse(query = "/data") |>
          as_tbl() |>
          map_na_if()
        ) |>
    _[[1]] |>
    set_names(fields(x))
}

method(quick_, pro_endpoint) <- function(x, offset, limit) {

  as.list(x@identifier) |>
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
             offset  = thresh(offset, d),
             limit   = thresh(limit, 2000L)
           )
    ) |>
    req_perform_parallel(on_error = "continue") |>
    map(\(x) resp_body_string(x) |>
          fparse() |>
          _[["results"]] |>
          as_tbl() |>
          map_na_if()
    ) |>
    _[[1]] |>
    set_names(x@dimensions@fields)
}

method(quick_, open_endpoint) <- function(x, offset, limit) {

  as.list(x@identifier) |>
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
             limit   = thresh(limit, 500L)
           )
    ) |>
    req_perform_parallel(on_error = "continue") |>
    map(\(x) resp_body_string(x) |>
          fparse() |>
          _[["results"]] |>
          as_tbl() |>
          map_na_if()
    ) |>
    _[[1]] |>
    set_names(x@dimensions@fields)
}

method(quick_, caid_endpoint) <- function(x, offset, limit) {

  as.list(x@identifier) |>
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
             limit   = thresh(limit, 8000L)
           )
    ) |>
    req_perform_parallel(on_error = "continue") |>
    map(\(x) resp_body_string(x) |>
          fparse() |>
          _[["results"]] |>
          as_tbl() |>
          map_na_if()
    ) |>
    _[[1]] |>
    set_names(x@dimensions@fields)
}

method(quick_, hgov_endpoint) <- function(x, offset, limit) {
  as.list(x@identifier) |>
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
             limit   = thresh(limit, 500L)
           )
    ) |>
    req_perform_parallel(on_error = "continue") |>
    map(\(x) resp_body_string(x) |>
          fparse() |>
          _[["results"]] |>
          as_tbl() |>
          map_na_if()
    ) |>
    _[[1]] |>
    set_names(x@dimensions@fields)
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
