#' @autoglobal
#' @noRd
fibble <- function(...) {
  fastplyr::new_tbl(...)
}

#' @autoglobal
#' @noRd
as_fibble <- function(x) {
  fastplyr::as_tbl(x)
}

#' @autoglobal
#' @noRd
flist <- function(...) {
  fastplyr::list_tidy(...)
}

#' @autoglobal
#' @noRd
ffill <- function(x,
                  ...,
                  by = NULL,
                  cols = NULL,
                  direction = "forwards",
                  limit = Inf,
                  new_names = "{.col}") {
  fastplyr::f_fill(
    data = x,
    ...,
    .by = by,
    .cols = cols,
    .direction = direction,
    .fill_limit = limit,
    .new_names = new_names
  )
}

#' @autoglobal
#' @noRd
fnest <- function(x,
                  ...,
                  add = FALSE,
                  by = NULL,
                  cols = NULL) {
  fastplyr::f_nest_by(
    data = x,
    ...,
    .add = add,
    .by = by,
    .cols = cols) |>
    fastplyr::f_ungroup()
}
