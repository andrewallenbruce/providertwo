#' @include S7_classes.R
#' @include S7_care.R
NULL

#' @autoglobal
#' @noRd
quick_cli_ <- function(x) {
  x <- unlist(x)
  glue::glue_col("{silver {names(x)}=}{red {bold {unname(x)}}}") |>
    glue::glue_collapse(sep = "&")
}

#' Quick Query
#' @param x An S7 `<endpoint>` object
#' @param ... Additional arguments
#' @returns A list of results from the API query, or `NULL` if no results are found.
#' @examples
#' asc <- pro_endpoint("asc_facility")
#'
#' q0 <- list(
#' `conditions[0][property]` = "state",
#' `conditions[0][operator]` = "=",
#' `conditions[0][value]` = "ZZ")
#'
#' q1 <- list(
#' `conditions[0][property]` = "state",
#' `conditions[0][operator]` = "=",
#' `conditions[0][value]` = "NY")
#'
#' q2 <- list(
#'   `conditions[0][property]` = "state",
#'   `conditions[0][operator]` = "IN",
#'   `conditions[0][value][1]` = "CA",
#'   `conditions[0][value][2]` = "GA",
#'   `conditions[0][value][3]` = "NY")
#'
#' q3 <- list(
#'   `conditions[0][property]` = "asc2_rate",
#'   `conditions[0][operator]` = "<",
#'   `conditions[0][value]` = "0.02")
#'
#' quick_query_(asc, q0)
#' quick_query_(asc, q1)
#' quick_query_(asc, q2)
#' quick_query_(asc, q3)
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
      !!!query
    ) |>
    perform_simple() |>
    _$count

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
            parse_string(x, query = "results") |>
            as_tbl() |>
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
        limit   = limit_(x),
        !!!query
      )
    paste0(base$url, "&offset=", offset_seq(n, limit_(x))) |>
      map(request) |>
      req_perform_parallel(on_error = "continue") |>
      map(function(x)
        parse_string(x, query = "results") |>
          as_tbl() |>
          map_na_if()) |>
      list_rbind() |>
      name_fields_(x)
  }
}
