#' @autoglobal
#' @noRd
bound <- function(lower, upper) {
  check_number_whole(lower, min = 0)
  # check_number_whole(upper, min = lower)
  cheapr_if_else(lower > upper, upper, lower)
}

#' @autoglobal
#' @noRd
parse_string <- function(resp, query = NULL) {
  fparse(resp_body_string(resp), query = query)
}

#' @autoglobal
#' @noRd
dimensions <- function(obj) {
  check_is_S7(obj)
  prop(obj, "dimensions") |>
    props(names = c("limit", "rows"))
}

#' @autoglobal
#' @noRd
fields <- function(obj) {
  check_is_S7(obj)
  prop(obj, "dimensions") |>
    prop("fields") |>
    names()
}

#' @autoglobal
#' @noRd
identifier <- function(obj) {
  check_is_S7(obj, class_endpoint)
  as.list(prop(obj, "identifier"))
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
          offset  = bound(offset, dimensions(x)$rows),
          limit   = bound(limit, dimensions(x)$limit)
        )
    ) |>
    req_perform_parallel(on_error = "continue") |>
    map(function(x)
      parse_string(x) |> _[["results"]] |>
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
          offset = bound(offset, dimensions(x)$rows),
          size   = bound(limit, dimensions(x)$limit)
        )) |>
    req_perform_parallel(on_error = "continue") |>
    map(function(x)
      parse_string(x, query = "/data") |>
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

method(quick_, class_temporal) <- function(x, offset, limit) {
  prop(x, "endpoints") |>
    get_elem("identifier") |>
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
          offset  = bound(offset, dimensions(x)$rows),
          limit   = bound(limit, dimensions(x)$limit)
        )
    ) |>
    req_perform_parallel(on_error = "continue") |>
    set_names(prop(x, "endpoints") |> get_elem("year")) |>
    map(function(resp)
      parse_string(resp) |> _[["results"]]) |>
    list_rbind(names_to = "year") |>
    map_na_if() |>
    as_tbl()
}

method(quick_, care_temporal) <- function(x, offset, limit) {
  prop(x, "endpoints") |>
    get_elem("identifier") |>
    map(function(i)
      request(i) |>
        req_url_query(
          offset = bound(offset, dimensions(x)$rows),
          limit  = bound(limit, dimensions(x)$limit))) |>
    req_perform_parallel(on_error = "continue") |>
    set_names(prop(x, "endpoints") |> get_elem("year")) |>
    map(parse_string) |>
    list_rbind(names_to = "year") |>
    map_na_if() |>
    as_tbl()
}
