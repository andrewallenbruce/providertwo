#' @autoglobal
#' @noRd
dimensions <- function(obj) {
  check_is_S7(obj)
  prop(obj, "dimensions") |>
    props(names = c("limit", "rows"))
}

#' @autoglobal
#' @noRd
fields <- function(obj) {
  check_is_S7(obj)
  prop(obj, "dimensions") |>
    prop("fields") |>
    names()
}

#' @autoglobal
#' @noRd
identifier <- function(obj) {
  check_is_S7(obj, class_endpoint)
  as.list(prop(obj, "identifier"))
}

#' @autoglobal
#' @noRd
members <- function(obj) {
  check_is_S7(obj, class_group)
  prop(obj, "members")
}

#' @autoglobal
#' @noRd
name_members <- function(x, obj) {
  set_names(x, names(members(obj)))
}

#' @autoglobal
#' @noRd
name_fields <- function(x, obj) {
  set_names(x, fields(obj))
}
