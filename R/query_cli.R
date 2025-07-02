#' @autoglobal
#' @noRd
fmt_right <- function(x) {
  format(x, justify = "right")
}

#' @autoglobal
#' @noRd
fmt_left <- function(x) {
  format(x, justify = "left")
}

#' @autoglobal
#' @noRd
brackets_cli <- function(x) {
  paste0(
    cli::col_silver("["),
    paste0(x, collapse = cli::col_silver(", ")),
    cli::col_silver("]")
  )
}


#' @autoglobal
#' @noRd
query_cli <- function(x) {

  if (any(is_named_rhs(x))) {

    x[is_named_rhs(x)] <- map(
      x[is_named_rhs(x)], function(x) {
        cli::col_green(deparse1(f_rhs(x)))
      })

  }

  if (any(is_len_two(x))) {

    x[is_len_two(x)] <- brackets(
      cli::col_yellow(
        unlist(
          x[is_len_two(x)],
          use.names = FALSE)
      )
    )
  }

  if (any(is_nil(x))) x[is_nil(x)] <- cli::col_red("NULL")

  if (any(is_len_one_not_nil(x))) {

    x[is_len_one_not_nil(x)] <- cli::col_yellow(
      unlist(
        x[is_len_one_not_nil(x)],
        use.names = FALSE)
    )
  }

  VALUE <- fmt_left(unlist(x, use.names = FALSE))

  FIELD <- fmt_right(names(x))

  EQUAL <- cli::col_black("=")

  glue("{FIELD} {EQUAL} {VALUE}")
}

# is_call(quote(starts_with_("Andr")))
# query_cli2(
#   first_name = starts_with_("Andr"),
#   last_name = contains_("J"),
#   state = in_(c("CA", "GA", "NY"), negate = TRUE),
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

  if (any(is_call(x))) {

    x[is_named_rhs(x)] <- map(
      x[is_named_rhs(x)], function(x) {
        cli::col_green(deparse1(f_rhs(x)))
      })

  }

  if (any(is_len_two(x))) {

    x[is_len_two(x)] <- brackets_cli(
      cli::col_yellow(
        unlist(
          x[is_len_two(x)],
          use.names = FALSE)
      )
    )
  }

  if (any(is_nil(x))) x[is_nil(x)] <- cli::col_red("NULL")

  if (any(is_len_one_not_nil(x))) {

    x[is_len_one_not_nil(x)] <- cli::col_yellow(
      unlist(
        x[is_len_one_not_nil(x)],
        use.names = FALSE)
    )
  }

  VALUE <- fmt_left(unlist(x, use.names = FALSE))

  FIELD <- fmt_right(names(x))

  EQUAL <- cli::col_black("=")

  glue("{FIELD} {EQUAL} {VALUE}")
}
