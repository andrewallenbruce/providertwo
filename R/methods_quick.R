#' @include S7_classes.R
#' @include S7_care.R
NULL

#' @autoglobal
#' @noRd
quick_ <- new_generic("quick_", "x", function(x, ..., offset, limit) {
  S7_dispatch()
})

method(quick_, class_endpoint) <- function(x, offset, limit) {
  identifier_(x) |>
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
          offset  = bound(offset, rows_(x)),
          limit   = bound(limit, limit_(x))
        )
    ) |>
    req_perform_parallel(on_error = "continue") |>
    map(function(x)
      parse_string(x, query = "results") |>
        as_tbl() |>
        map_na_if()) |>
    pluck(1) |>
    name_fields_(x)
}

method(quick_, care_endpoint) <- function(x, offset, limit) {
  identifier_(x) |>
    map(function(i)
      request(i) |>
        req_url_query(
          offset = bound(offset, rows_(x)),
          size   = bound(limit, limit_(x))
        )
      ) |>
    req_perform_parallel(on_error = "continue") |>
    map(function(x)
      parse_string(x, query = "/data") |>
        as_tbl() |>
        map_na_if()) |>
    pluck(1) |>
    name_fields_(x)
}

method(quick_, class_group) <- function(x, offset, limit) {
  members_(x) |>
    map(function(x)
      quick_(x, offset, limit), .progress = TRUE) |>
    name_members_(x)
}

method(quick_, class_temporal) <- function(x, offset, limit) {
  identifier_(x) |>
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
          offset  = bound(offset, rows_(x)),
          limit   = bound(limit, limit_(x))
        )
    ) |>
    req_perform_parallel(on_error = "continue") |>
    name_years_(x) |>
    map(function(resp)
      parse_string(resp, query = "results")) |>
    list_rbind(names_to = "year") |>
    map_na_if() |>
    as_tbl()
}

method(quick_, care_temporal) <- function(x, offset, limit) {
  identifier_(x) |>
    map(function(i)
      request(i) |>
        req_url_query(
          offset = bound(offset, rows_(x)),
          size   = bound(limit, limit_(x))
        )
      ) |>
    req_perform_parallel(on_error = "continue") |>
    name_years_(x) |>
    map(parse_string) |>
    list_rbind(names_to = "year") |>
    map_na_if() |>
    as_tbl()
}
