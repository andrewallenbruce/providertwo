# x <- exprs(
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

  q <- process_query(..., .type = .type)

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
