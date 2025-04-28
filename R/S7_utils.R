#' @autoglobal
#' @noRd
total_rows <- function(x) {
  prop(x, "dimensions") |>
    prop("rows")
}

#' @autoglobal
#' @noRd
prop_empty <- function(obj, nm) {
  check_is_S7(obj)
  empty(prop(obj, nm))
}
