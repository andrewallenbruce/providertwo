#' @include classes.R
NULL

#' @autoglobal
#' @noRd
quick_ <- new_generic("quick_", "x", function(x, ..., offset, limit) {
  S7_dispatch()
})

method(quick_, current) <- function(x, offset, limit) {

  switch(
    clog_(x),
    care = map(identifier_(x), function(i)
      request(i) |>
        req_url_query(
          offset = bound(offset, rows_(x)),
          size   = bound(limit, limit_(x))
        )) |>
      req_perform_parallel(on_error = "continue") |>
      map(
        function(x)
          parse_string(x, query = "/data") |>
          as_fibble() |>
          map_na_if()
      ) |>
      pluck(1) |>
      name_fields_(x),
    caid = ,
    prov  = ,
    open = ,
    hgov = identifier_(x) |>
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
      map(
        function(x)
          parse_string(x, query = "results") |>
          as_fibble() |>
          map_na_if()
      ) |>
      pluck(1) |>
      name_fields_(x)
  )
}

method(quick_, temporal) <- function(x, offset, limit) {
  switch(
    clog_(x),
    care = identifier_(x) |>
      map(
        function(i)
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
      as_fibble(),
    caid = ,
    prov = ,
    open = ,
    hgov = identifier_(x) |>
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
      map(function(x)
        parse_string(x, query = "results")) |>
      list_rbind(names_to = "year") |>
      map_na_if() |>
      as_fibble()
  )
}

method(quick_, class_group) <- function(x, offset, limit) {
  members_(x) |>
    map(function(x)
      quick_(x, offset, limit), .progress = TRUE) |>
    name_members_(x)
}

#' Quickly access CMS data
#'
#' Convenience function to quickly access various CMS data endpoints.
#' Mostly for debugging purposes.
#'
#' @param alias  `<chr>` Alias representing the CMS data endpoint or category.
#' @param offset `<int>` The offset for pagination. Default is `0`.
#' @param limit  `<int>` The maximum number of records to retrieve. Default is `10000`.
#' @returns A data frame containing the requested CMS data.
# quick("revalid_group")
# quick("out_img_national")
# quick("hgov_nipr_state")
#' @autoglobal
#' @noRd
quick <- function(alias,
                  offset = 0,
                  limit  = 1e4) {

  check_required(alias)

  quick_(
    endpoint(alias),
    offset = offset,
    limit  = limit)
}

#' @include classes.R
NULL

#' @autoglobal
#' @noRd
quick_cli_ <- function(x) {
  glue_col(
    "{silver {names(x)}=}",
    "{red {bold {unname(x)}}}",
    x = unlist(x)) |>
    glue_collapse(sep = "&")
}

#' Quick Query
#' @param x An S7 `<endpoint>` object
#' @param ... Additional arguments
#' @returns A list of results from the API query, or `NULL` if no results are found.
# asc <- endpoint("asc_facility")
#
# q0 <- list(
# `conditions[0][property]` = "state",
# `conditions[0][operator]` = "=",
# `conditions[0][value]` = "ZZ")
#
# q1 <- list(
# `conditions[0][property]` = "state",
# `conditions[0][operator]` = "=",
# `conditions[0][value]` = "NY")
#
# q2 <- list(
#   `conditions[0][property]` = "state",
#   `conditions[0][operator]` = "IN",
#   `conditions[0][value][1]` = "CA",
#   `conditions[0][value][2]` = "GA",
#   `conditions[0][value][3]` = "NY")
#
# q3 <- list(
#   `conditions[0][property]` = "asc2_rate",
#   `conditions[0][operator]` = "<",
#   `conditions[0][value]` = "0.02")
#
# quick_query_(asc, q0)
# quick_query_(asc, q1)
# quick_query_(asc, q2)
# quick_query_(asc, q3)
#' @autoglobal
#' @export
quick_query_ <- new_generic("quick_query_", "x")

method(quick_query_, class_endpoint) <- function(x, query = NULL) {
  n <- identifier_(x) |>
    request() |>
    req_url_query(
      count   = "true",
      format  = "json",
      keys    = "true",
      results = "false",
      rowIds  = "false",
      schema  = "false",
      offset  = 0L,
      limit   = 1L,
      splice(query)
    ) |>
    perform_simple()

  n <- switch(
    clog_(x),
    care = get_elem(n, "total_rows"),
    prov = get_elem(n, "query", invert = TRUE))

  if (n == 0L) {
    cli_alert_danger(
      "Query {.var {quick_cli_(query)}} returned {.emph 0} results.",
      wrap = TRUE)
    return(invisible(NULL))
  }

  if (n <= limit_(x)) {
    cli_alert_success("Returning {.emph {n}} results...", wrap = TRUE)
    return(
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
              offset  = 0L,
              limit   = limit_(x),
              !!!query
            )
        ) |>
        req_perform_parallel(on_error = "continue") |>
        map(
          function(x)
            parse_string(x, query = "results") |> # TODO
            as_fibble() |>
            map_na_if()
        ) |>
        pluck(1) |>
        name_fields_(x)
    )
  }

  if (n > limit_(x)) {
    cli_alert_success("Returning {.emph {n}} results...", wrap = TRUE)
    base <- identifier_(x) |>
      request() |>
      req_url_query(
        count   = "false",
        format  = "json",
        keys    = "true",
        results = "true",
        rowIds  = "false",
        schema  = "false",
        size    = limit_(x),
        limit   = limit_(x),
        !!!query)

    paste0(
      base$url,
      "&offset=",
      offset_seq(
        bound(n, 50000), # TODO
        limit_(x))) |>   # TODO
      map(request) |>
      req_perform_parallel(on_error = "continue") |>
      map(
        function(x)
          parse_string(x,
            query = "/data") |>  # TODO
          as_fibble() |>
          map_na_if()) |>
      list_rbind() |>
      name_fields_(x)
  }
}
