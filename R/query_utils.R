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
is_mod <- function(x) {
  rlang::is_call(
    x,
    name = c(
      "any_of",
      "none_of",
      "equal",
      "not_equal",
      "between",
      "not_between",
      "greater_than",
      "less_than",
      "starts_with",
      "ends_with",
      "contains",
      "like"
    )
  )
}

#' @autoglobal
#' @noRd
is_junc <- function(x) {
  rlang::is_call(x, name = c("and", "or"))
}

#' @autoglobal
#' @noRd
any_junc <- function(x) {
  any(purrr::map_lgl(x, \(x) is_junc(x)), na.rm = TRUE)
}

#' @autoglobal
#' @noRd
are_mods <- function(x) {
  purrr::map_lgl(x, is_mod)
}

#' @autoglobal
#' @noRd
any_mods <- function(x) {
  any(are_mods(x))
}

#' @autoglobal
#' @noRd
deparse_mods <- function(x) {
  purrr::map(x[are_mods(x)], function(x) deparse1(x))
}

#' @autoglobal
#' @noRd
are_calls <- function(x) {
  purrr::map_lgl(x, function(x) Negate(is_mod)(x) & rlang::is_call(x))
}

#' @autoglobal
#' @noRd
any_calls <- function(x) {
  any(are_calls(x))
}

#' @autoglobal
#' @noRd
deparse_calls <- function(x) {
  purrr::map(x[are_calls(x)], function(x) deparse1(x))
}

#' @autoglobal
#' @noRd
are_evaled <- function(x) {
  cheapr::list_lengths(x) >= 1L & purrr::map_lgl(x, Negate(is_call))
}

#' @autoglobal
#' @noRd
any_evaled <- function(x) {
  any(are_evaled(x))
}

#' @autoglobal
#' @noRd
eval_cli <- function(x) {
  purrr::map(x[are_evaled(x)], function(x)
    cli::col_cyan(paste0(glue::double_quote(x), collapse = ", ")))
}

#' @autoglobal
#' @noRd
call_cli <- function(x) {
  purrr::map(x[are_calls(x)], function(x) cli::col_yellow(deparse1(x)))
}

#' @autoglobal
#' @noRd
mods_cli <- function(x) {
  purrr::map(x[are_mods(x)], function(x) cli::col_red(deparse1(x)))
}

#' @autoglobal
#' @noRd
are_length_one <- function(x) {
  cheapr::list_lengths(x) == 1L
}

#' @autoglobal
#' @noRd
are_length_two <- function(x) {
  cheapr::list_lengths(x) > 1L
}

#' @autoglobal
#' @noRd
any_length_two <- function(x) {
  any(are_length_two(x))
}

#' @autoglobal
#' @noRd
are_null <- function(x) {
  purrr::map_lgl(x, is.null)
}

#' @autoglobal
#' @noRd
any_null <- function(x) {
  any(are_null(x))
}

#' @autoglobal
#' @noRd
are_not_null <- function(x) {
  purrr::map_lgl(x, Negate(is.null))
}

# x <- new_query(
#   first_name = starts_with("Andr"),
#   last_name  = contains("J"),
#   state      = any_of(c("CA", "GA", "NY")),
#   state_own  = none_of(c("GA", "MD")),
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
  cli::cat_print(glue::glue_safe("{FIELD} {EQUALS} {VALUE}"))

  invisible(x)
}
