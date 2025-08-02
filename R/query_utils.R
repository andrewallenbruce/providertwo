#' @autoglobal
#' @noRd
flatten_query <- function(x) {
  # purrr::map(x, paste0, collapse = "&")
  purrr::map(x, function(x)
    paste0(x, collapse = "&")) |>
    unlist(use.names = FALSE) |>
    paste0(collapse = "&")
}

#' @autoglobal
#' @noRd
brackets <- function(x) {
  paste0("[", x, "]")
}

#' @autoglobal
#' @noRd
just_right <- function(x) {
  format(x, justify = "right")
}

#' @autoglobal
#' @noRd
just_left <- function(x) {
  format(x, justify = "left")
}

#' @autoglobal
#' @noRd
eval_cli <- function(x) {
  map(x[are_evaled(x)],
      function(x)
        cli::ansi_collapse(
          cli::col_cyan(
            paste0(x, collapse = ", ")
            ),
          trunc = 5)
      )
  # |> cli::ansi_strtrim(25)
}

#' @autoglobal
#' @noRd
brackets_cli <- function(x) {
  paste0(
    cli::col_silver("["),
    paste0(
      cli::col_yellow((unlist(x, use.names = FALSE))
    ),
    collapse = cli::col_silver(", ")),
    cli::col_silver("]")
  )
}

#' @autoglobal
#' @noRd
brackets_cli2 <- function(x) {
  paste0(
    cli::col_black("["),
    cli::col_silver(x),
    cli::col_black("]")
  )
}

#' @autoglobal
#' @noRd
call_cli <- function(x) {
  map(x[are_calls(x)], function(x) cli::col_yellow(deparse1(x)))
}

#' @autoglobal
#' @noRd
mods_cli <- function(x) {
  map(x[are_mods(x)], function(x) cli::col_red(deparse1(x)))
}

#' @autoglobal
#' @noRd
deparse_mods <- function(x) {
  map(x[are_mods(x)], function(x) deparse1(x))
}

#' @autoglobal
#' @noRd
deparse_calls <- function(x) {
  map(x[are_calls(x)], function(x) deparse1(x))
}

#' @autoglobal
#' @noRd
brack_along <- function(x) {
  if (length(x) > 1) {
    brackets(seq_along(x))
  } else {
    NULL
  }
}

#' @autoglobal
#' @noRd
are_length_one <- function(x) {
  list_lengths(x) == 1L
}

#' @autoglobal
#' @noRd
are_length_two <- function(x) {
  list_lengths(x) > 1L
}

#' @autoglobal
#' @noRd
are_null <- function(x) {
  map_lgl(x, is.null)
}

#' @autoglobal
#' @noRd
are_not_null <- function(x) {
  map_lgl(x, Negate(is.null))
}

#' @autoglobal
#' @noRd
is_mod <- function(x) {
  is_call(
    x,
    name = c(
      "between",
      "contains",
      "ends_with",
      "equals",
      "greater_than",
      "any_of",
      "less_than",
      "like",
      "starts_with"
    )
  )
}

#' @autoglobal
#' @noRd
are_mods <- function(x) {
  map_lgl(x, is_mod)
}

#' @autoglobal
#' @noRd
are_calls <- function(x) {
  map_lgl(x, function(x) Negate(is_mod)(x) & is_call(x))
}

#' @autoglobal
#' @noRd
any_calls <- function(x) {
  any(are_calls(x))
}

#' @autoglobal
#' @noRd
are_evaled <- function(x) {
  list_lengths(x) >= 1L & map_lgl(x, Negate(is_call))
}

#' @autoglobal
#' @noRd
any_evaled <- function(x) {
  any(are_evaled(x))
}

#' @autoglobal
#' @noRd
any_mods <- function(x) {
  any(are_mods(x))
}

#' @autoglobal
#' @noRd
any_length_two <- function(x) {
  any(are_length_two(x))
}

#' @autoglobal
#' @noRd
any_null <- function(x) {
  any(are_null(x))
}

# x <- new_query(
#   first_name = starts_with("Andr"),
#   last_name  = contains("J"),
#   state      = any_of(c("CA", "GA", "NY")),
#   city       = equals(c("Atlanta", "Los Angeles"), negate = TRUE),
#   state_own  = c("GA", "MD"),
#   npi        = npi_ex$k,
#   ccn        = "01256",
#   pac        = NULL,
#   rate       = between(0.45, 0.67),
#   year       = 2014:2025)
#
#   cli_query(x)
#' @autoglobal
#' @noRd
cli_query <- function(x) {

  x <- x@input

  if (any_evaled(x)) x[are_evaled(x)] <- eval_cli(x[are_evaled(x)])
  if (any_mods(x))   x[are_mods(x)]   <- mods_cli(x[are_mods(x)])
  if (any_calls(x))  x[are_calls(x)]  <- call_cli(x[are_calls(x)])

  FIELD  <- just_right(names(x))
  EQUALS <- cli::col_black(cli::style_bold(cli::symbol$double_line))
  VALUE  <- just_left(unlist(x, use.names = FALSE))

  cli::cli_h1("New Query:")
  glue::glue_safe("{FIELD} {EQUALS} {VALUE}")

  invisible(x)
}
