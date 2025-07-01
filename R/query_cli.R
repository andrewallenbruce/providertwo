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
brackets <- function(x) {
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
