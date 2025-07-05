# q <- create_query(
#   first_name = starts_with_("Andr"),
#   last_name = contains_("J"),
#   state = in_(c("CA", "GA", "NY")),
#   country = not_in_(c("CA", "GA", "NY")),
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

# g <- params_cli(
#   first_name = starts_with_("Andr"),
#   last_name = contains_("J"),
#   state = in_(c("CA", "GA", "NY")),
#   country = not_in_(c("CA", "GA", "NY")),
#   owner = c("GA", "MD"),
#   npi = npi_ex$k,
#   ccn = "01256",
#   pac = NULL
# )
#' @autoglobal
#' @noRd
params_cli <- function(...) {

  x <- enexprs(...)

  if (any_mods(x)) x[are_mods(x)] <- mods_cli(x[are_mods(x)])

  if (any_calls(x)) x[are_calls(x)] <- calls_cli(x[are_calls(x)])

  if (any_length_two(x)) {
    x[are_length_two(x)] <- brackets_cli(x[are_length_two(x)])
  }

  if (any_null(x)) x[are_null(x)] <- NULL #cli::col_silver("NULL")

  if (any_lone_not_null(x)) {
    x[are_lone_not_null(x)] <- lone_not_null_cli(x[are_lone_not_null(x)])
  }

  NUM    <- brackets_cli2(just_left(seq_along(names(x))))

  FIELD  <- just_right(names(x))

  EQUALS <- cli::col_black(cli::symbol$double_line)

  VALUE  <- just_left(unlist(x, use.names = FALSE))

  g <- glue("{NUM} {FIELD} {EQUALS} {VALUE}")

  cat(g, sep = "\n")

  invisible(g)
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

  cli::cli_h3(cli::col_yellow("Parameter Inputs:"))
  g <- params_cli(...)

  q <- create_query(..., .type = .type)

  cli::cli_h3(cli::col_yellow("Query Output:"))
  # map(q, function(x) unlist(x, use.names = FALSE) |> append("\n")) |>
  #   unlist(use.names = FALSE) |>
  #   cli::col_silver() |>
  #   cat(sep = "\n")
  strwrap(
    strsplit(flatten_query(q), "&") |>
      unlist(),
    width = 70,
    prefix = paste0(cli::symbol$full_block, cli::symbol$upper_block_1, " ")
  ) |> cli::col_silver() |> cat(sep = "\n")

  invisible(flatten_query(q))

}

# purrr::imap_chr(g, \(x, idx) paste0(idx, ": ", x)) |> cat(sep = "\n")
# cli::cli_div(theme = list(ol = list("margin-left" = 2)))
# cli::cli_ol()
# cli::cli_li("one")
# cli::cli_ul(c("foo", "bar", "foobar"))
# cli::cli_li("two")
# cli::cli_end()
# cli::cli_end()
