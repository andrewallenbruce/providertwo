#' @include S7_classes.R
NULL

#' @autoglobal
#' @noRd
endpoints_ <- new_generic("endpoints_", "obj", function(obj) {
  S7_dispatch()
})

method(endpoints_, class_temporal) <- function(obj) {
  prop(obj, "endpoints")
}

#' @autoglobal
#' @noRd
years_ <- new_generic("years_", "obj", function(obj) {
  S7_dispatch()
})

method(years_, class_temporal) <- function(obj) {
  endpoints_(obj) |> get_elem("year")
}

#' @autoglobal
#' @noRd
members_ <- new_generic("members_", "obj", function(obj) {
  S7_dispatch()
})

method(members_, class_group) <- function(obj) {
  prop(obj, "members")
}

#' @autoglobal
#' @noRd
dimensions_ <- new_generic("dimensions_", "obj", function(obj) {
  S7_dispatch()
})

method(dimensions_, class_backend) <- function(obj) {
  prop(obj, "dimensions")
}

method(dimensions_, class_group) <- function(obj) {
  members_(obj) |> map(dimensions_)
}

#' @autoglobal
#' @noRd
rows_ <- new_generic("rows_", "obj", function(obj) {
  S7_dispatch()
})

method(rows_, class_backend) <- function(obj) {
  dimensions_(obj) |> prop("rows")
}

method(rows_, class_group) <- function(obj) {
  members_(obj) |> map(rows_)
}

#' @autoglobal
#' @noRd
limit_ <- new_generic("limit_", "obj", function(obj) {
  S7_dispatch()
})

method(limit_, class_backend) <- function(obj) {
  dimensions_(obj) |> prop("limit")
}

method(limit_, class_group) <- function(obj) {
  members_(obj) |> map(limit_)
}

# fields_ <- new_generic("fields_", "obj")
# identifier_ <- new_generic("identifier_", "obj")
# identifiers_ <- new_generic("identifiers_", "obj")

# obj <- care_endpoint("care_dialysis")
# obj <- care_temporal("quality_payment")
# obj <- care_group("care_hospital")
#
# dimensions_(obj)
# rows_(obj)
# limit_(obj)
# members_(obj)
