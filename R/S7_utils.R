#' @autoglobal
#' @noRd
set_member_names <- function(x, obj) {
  set_names(x, names(obj))
}

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
members_names <- function(x) {
  members(x) |> names()
}

#' @autoglobal
#' @noRd
identifier <- function(x) {
  prop(x, "identifier")
}

#' @autoglobal
#' @noRd
resources <- function(x) {
  prop(x, "resources")
}

#' @autoglobal
#' @noRd
rows <- function(x) {
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
