#' @include S7_classes.R
#' @include S7_care.R
NULL

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
      parse_string(x, query = "results") |>
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
      parse_string(resp, query = "results")) |>
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
