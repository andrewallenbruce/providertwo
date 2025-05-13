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
resources <- function(x) {
  prop(x, "resources")
}

#' @autoglobal
#' @noRd
prop_empty <- function(obj, nm) {
  check_is_S7(obj)
  empty(prop(obj, nm))
}
