#' @autoglobal
#' @noRd
flatten_query <- function(x) {
  map(x, \(x) paste0(x, collapse = "&")) |>
    unlist(use.names = FALSE) |>
    paste0(collapse = "&")
}

# create_query(
#   first_name = starts_with_("Andr"),
#   last_name = contains_("J"),
#   state = in_(c("CA", "GA", "NY"), negate = TRUE),
#   country = in_(c("CA", "GA", "NY")),
#   owner = c("GA", "MD"),
#   npi = npi_ex$k,
#   ccn = "01256",
#   pac = NULL,
#   .type = "medicare"
# )
#' @autoglobal
#' @noRd
create_query <- function(..., .type) {

  args <- dots_list(..., .homonyms = "error")

  .c(VERB, FIELD, OPERATOR, VALUE, IDX, BDX) %=% query_keywords(type = .type)

  discard(args, is.null) |>
    imap(
      function(x, name) {

        p <- paste0(VERB, BDX, FIELD, name)
        o <- paste0(VERB, BDX, OPERATOR, if (is_modifier(x)) x[["operator"]] else "=")
        v <- if (is_modifier(x)) x[["value"]] else unlist(x, use.names = FALSE)

        if (length(v) > 1)

          v <- paste0(VERB, BDX, VALUE, "[", seq_along(v), "]=", v)

        if (length(v) == 1)

          v <- paste0(VERB, BDX, VALUE, "=", v)

        c(p, o, v)

      }) |>
    unname() |>
    imap(
      function(x, idx) {

        greplace(x, IDX, if (.type == "default") idx - 1 else idx)

      })
}

# query_cli2(
#   first_name = starts_with_("Andr"),
#   last_name = contains_("J"),
#   state = in_(c("CA", "GA", "NY")),
#   country = in_(c("CA", "GA", "NY")),
#   state_owner = c("GA", "MD"),
#   npi = npi_ex$k,
#   ccn = "01256",
#   pac = NULL,
#   .type = "medicare"
# )
#' @autoglobal
#' @noRd
query_cli2 <- function(...) {

  x <- enexprs(...)

  if (any_calls(x)) {

    x[are_calls(x)] <- map(
      x[are_calls(x)],
      function(x) {
        cli::col_green(deparse1(x))
      })

  }

  if (any_length_two(x)) {

    x[are_length_two(x)] <- brackets_cli(
      cli::col_yellow(
        unlist(
          x[are_length_two(x)],
          use.names = FALSE)
      )
    )
  }

  if (any_null(x)) x[are_null(x)] <- cli::col_red("NULL")

  if (any_length_one_not_null(x)) {

    x[are_length_one_not_null(x)] <- cli::col_yellow(
      unlist(
        x[are_length_one_not_null(x)],
        use.names = FALSE)
    )
  }

  VALUE <- fmt_left(unlist(x, use.names = FALSE))

  FIELD <- fmt_right(names(x))

  EQUAL <- cli::col_black("=")

  glue("{FIELD} {EQUAL} {VALUE}")
}

# new_query(
#   first_name = starts_with_("Andr"),
#   last_name = contains_("J"),
#   state = in_(c("CA", "GA", "NY")),
#   country = in_(c("CA", "GA", "NY")),
#   state_owner = c("GA", "MD"),
#   npi = npi_ex$k,
#   ccn = "01256",
#   pac = NULL,
#   .type = "medicare"
# )
#' @autoglobal
#' @noRd
new_query <- function(..., .type) {

  if (is_missing(.type)) .type <- "default"

  cat(query_cli2(...), sep = "\n")
  cat("\n")

  qry <- create_query(..., .type = .type)

  map(qry, function(x) {
    unlist(x) |>
      append("\n")
  }) |>
    unlist() |>
    cat(sep = "\n")

  invisible(flatten_query(qry))

}
