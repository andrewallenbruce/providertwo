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
lone_not_null_cli <- function(x) {
  cli::col_yellow(unlist(x, use.names = FALSE))
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
calls_cli <- function(x) {
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
  map(x[are_mods(x)], \(x) deparse1(x))
}

#' @autoglobal
#' @noRd
deparse_calls <- function(x) {
  map(x[are_calls(x)], \(x) deparse1(x))
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
      "between_",
      "contains_",
      "ends_with_",
      "equals_",
      "greater_than_",
      "in_",
      "less_than_",
      "like_",
      "starts_with_"
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
  map_lgl(x, is_call)
}

#' @autoglobal
#' @noRd
any_calls <- function(x) {
  any(are_calls(x))
}

# are_calls <- function(x) {
#   map_lgl(x, \(x) purrr::negate(is_mod)(x) & is_call(x))
# }

#' @autoglobal
#' @noRd
are_lone_not_null <- function(x) {
  are_length_one(x) & are_not_null(x)
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

#' @autoglobal
#' @noRd
any_lone_not_null <- function(x) {
  any(are_lone_not_null(x))
}

# seq_along0 <- function(x) {
#   seq_along(x) - 1
#   0L:(length(x) - 1L)
# }

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
