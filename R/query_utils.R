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
seq_along0 <- function(x) {
  seq_along(x) - 1
  # 0L:(length(x) - 1L)
}

#' @autoglobal
#' @noRd
is_len_one <- function(x) {
  # map_lgl(x, \(x) length(x) == 1L)
  list_lengths(x) == 1L
}

#' @autoglobal
#' @noRd
is_len_two <- function(x) {
  # map_lgl(x, \(x) length(x) > 1L)
  list_lengths(x) > 1L
}

#' @autoglobal
#' @noRd
is_nil <- function(x) {
  map_lgl(x, is.null)
}

#' @autoglobal
#' @noRd
is_not_nil <- function(x) {
  map_lgl(x, Negate(is.null))
}

#' @autoglobal
#' @noRd
is_len_one_not_nil <- function(x) {
  is_len_one(x) & is_not_nil(x)
}

#' @autoglobal
#' @noRd
is_full_formula <- function(x) {
  map_lgl(x, is_formula, lhs = TRUE)
}

#' @autoglobal
#' @noRd
is_rhs_formula <- function(x) {
  map_lgl(x, is_formula, lhs = FALSE)
}

#' @autoglobal
#' @noRd
is_unnamed_formula <- function(x) {
  !have_name(x) & is_full_formula(x)
}

#' @autoglobal
#' @noRd
is_named_rhs <- function(x) {
  have_name(x) & is_rhs_formula(x)
}

# x <- list(
#   first ~ starts_with_("Andr"),
#   first = ~starts_with_("Andr"),
#   first = "Andrew")
# # MISSES the unnamed formula
# check_names_unique(x)
#' @autoglobal
#' @noRd
check_names_unique <- function(x, call = caller_env()) {

  if (anyDuplicated(names(x[have_name(x)]))) {

    x <- names(x[have_name(x)])
    i <- which_(fduplicated(x))

    cli_abort(
      c("x" = "Field{?s} {.field {x[i]}} appea{?rs/rs/r} multiple times."),
      # TODO add more info ala check_unnamed_formula
      call = call)
  }
}

# x <- list(first ~ starts_with_("Andr"))
# check_unnamed_formula(x)
#' @autoglobal
#' @noRd
check_unnamed_formula <- function(x, call = caller_env()) {

  if (any(is_unnamed_formula(x))) {
    x <- map(
      x[is_unnamed_formula(x)],
      function(x)
        as_label(x)
      ) |>
      unlist(use.names = FALSE) |>
      set_names("*")

    cli_abort(
      c("x" = "{.emph {length(x)} Unnamed formula{?s}} detected:",
        cli::col_yellow(cli::format_bullets_raw(x))),
      call = call)
  }
}

# x <- list(state = ~ in_(c("CA", "GA", "NY")))
# convert_named_formula(x)
#' @autoglobal
#' @noRd
convert_named_formula <- function(x) {
  if (any(is_named_rhs(x))) {
    # x[is_named_rhs(x)] <- map(x[is_named_rhs(x)], f_rhs)
    x[is_named_rhs(x)] <- map(
      x[is_named_rhs(x)],
      function(x)
        f_rhs(x) |>
        eval_bare()
      )
    }
  x
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

# x <- list(last_name ~ contains_("J"))
# convert_unnamed_formula(x)
#' @autoglobal
#' @noRd
convert_unnamed_formula <- function(x) {

  if (any(is_unnamed_formula(x))) {

    i   <- is_unnamed_formula(x)
    tmp <- x[i]
    rhs <- map(tmp, f_rhs)
    lhs <- map(tmp, function(x)
      f_lhs(x) |> as_string()) |>
      list_c()

    x[i] <- rhs

    names(x)[i] <- lhs
  }
  x
}
