#' @autoglobal
#' @noRd
seq_along0 <- function(x) {
  seq_along(x) - 1
  # 0L:(length(x) - 1L)
}

#' @autoglobal
#' @noRd
is_length_one <- function(x) {
  # map_lgl(x, \(x) length(x) == 1L)
  list_lengths(x) == 1L
}

#' @autoglobal
#' @noRd
is_length_two <- function(x) {
  # map_lgl(x, \(x) length(x) > 1L)
  list_lengths(x) > 1L
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
is_unnamed_full_formula <- function(x) {
  !have_name(x) & is_full_formula(x)
}

#' @autoglobal
#' @noRd
is_named_rhs_formula <- function(x) {
  have_name(x) & is_rhs_formula(x)
}

# x <- list(
# first ~ starts_with_("Andr"),
# last_name ~ contains_("J"))
# convert_unnamed_formula(x)
#' @autoglobal
#' @noRd
convert_unnamed_formula <- function(x) {

  if (any(is_unnamed_full_formula(x))) {

    i <- is_unnamed_full_formula(x)

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
