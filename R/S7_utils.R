#' @autoglobal
#' @noRd
dimensions <- function(x) {
  prop(x, "dimensions")
}

#' @autoglobal
#' @noRd
endpoints <- function(x) {
  prop(x, "endpoints")
}

#' @autoglobal
#' @noRd
members <- function(x) {
  prop(x, "members")
}

#' @autoglobal
#' @noRd
identifier <- function(x) {
  prop(x, "identifier")
}

#' @autoglobal
#' @noRd
total_rows <- function(x) {
  dimensions(x) |> prop("rows")
}

#' @autoglobal
#' @noRd
fields <- function(x) {
  dimensions(x) |> prop("fields")
}

#' @autoglobal
#' @noRd
prop_empty <- function(obj, nm) {
  check_is_S7(obj)
  empty(prop(obj, nm))
}
