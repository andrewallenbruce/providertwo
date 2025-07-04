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

# query_cli(
#   first_name = starts_with_("Andr"),
#   last_name = contains_("J"),
#   state = in_(c("CA", "GA", "NY")),
#   country = in_(c("CA", "GA", "NY")),
#   state_owner = c("GA", "MD"),
#   npi = npi_ex$k,
#   ccn = "01256",
#   pac = NULL
# )
#' @autoglobal
#' @noRd
query_cli <- function(...) {

  x <- enexprs(...)

  if (any_calls(x)) {
    x[are_calls(x)] <- calls_cli(x[are_calls(x)])
  }

  if (any_length_two(x)) {
    x[are_length_two(x)] <- brackets_cli(x[are_length_two(x)])
  }

  if (any_null(x)) x[are_null(x)] <- cli::col_red("NULL")

  if (any_lone_not_null(x)) {
    x[are_lone_not_null(x)] <- lone_not_null_cli(x[are_lone_not_null(x)])
  }

  FIELD  <- just_right(names(x))

  EQUALS <- cli::col_black("=")

  VALUE  <- just_left(unlist(x, use.names = FALSE))

  glue("{FIELD} {EQUALS} {VALUE}")
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

  cat(query_cli(...), sep = "\n")
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
