#' @include classes.R
NULL

#--------- class_group -----------
#' @autoglobal
#' @noRd
# members_ <- new_generic("members_", "obj", function(obj) {
#   S7_dispatch()
# })
#
# method(members_, class_group) <- function(obj) {
#   prop(obj, "members")
# }

#' @autoglobal
#' @noRd
#' member_names_ <- new_generic("member_names_", "obj", function(obj) {
#'   S7_dispatch()
#' })
#'
#' method(member_names_, class_group) <- function(obj) {
#'   members_(obj) |> names()
#' }
#'
#' #' @autoglobal
#' #' @noRd
#' name_members_ <- function(x, obj) {
#'   set_names(x, member_names_(obj))
#' }

#--------- class_temporal ---------
#' @autoglobal
#' @noRd
# endpoints_ <- new_generic("endpoints_", "obj", function(obj) {
#   S7_dispatch()
# })
#
# method(endpoints_, class_temporal) <- function(obj) {
#   prop(obj, "endpoints")
# }
#
# method(endpoints_, class_endpoint) <- function(obj) {
#   NULL
# }
#
# method(endpoints_, class_group) <- function(obj) {
#   members_(obj) |> map(endpoints_)
# }

#' @autoglobal
#' @noRd
#' years_ <- new_generic("years_", "obj", function(obj) {
#'   S7_dispatch()
#' })
#'
#' method(years_, class_temporal) <- function(obj) {
#'   endpoints_(obj) |> get_elem("year")
#' }
#'
#' method(years_, class_endpoint) <- function(obj) {
#'   NULL
#' }
#'
#' method(years_, class_group) <- function(obj) {
#'   members_(obj) |> map(years_)
#' }
#'
#' #' @autoglobal
#' #' @noRd
#' name_years_ <- function(x, obj) {
#'   set_names(x, years_(obj))
#' }

#--------- class_backend ---------

#--------- metadata --------------
#' @autoglobal
#' @noRd
# metadata_ <- new_generic("metadata_", "obj", function(obj) {
#   S7_dispatch()
# })
#
# method(metadata_, class_backend) <- function(obj) {
#   prop(obj, "metadata")
# }
#
# method(metadata_, class_group) <- function(obj) {
#   members_(obj) |> map(metadata_)
# }

#' @autoglobal
#' @noRd
# api_ <- new_generic("api_", "obj", function(obj) {
#   S7_dispatch()
# })
#
# method(api_, class_list) <- function(obj) {
#   obj |> get_elem("api")
# }
#
# method(api_, class_backend) <- function(obj) {
#   metadata_(obj) |> get_elem("api")
# }
#
# method(api_, class_group) <- function(obj) {
#   members_(obj) |> map(api_)
# }

#' @autoglobal
#' @noRd
# clog_ <- new_generic("clog_", "obj", function(obj) {
#   S7_dispatch()
# })
#
# method(clog_, class_list) <- function(obj) {
#   obj |> get_elem("clog")
# }
#
# method(clog_, class_backend) <- function(obj) {
#   metadata_(obj) |> get_elem("clog")
# }
#
# method(clog_, class_group) <- function(obj) {
#   members_(obj) |> map(clog_)
# }

#' @autoglobal
#' @noRd
# resources_ <- new_generic("resources_", "obj", function(obj) {
#   S7_dispatch()
# })
#
# method(resources_, class_endpoint) <- function(obj) {
#   if (clog_(obj) != "care") return(NULL)
#   metadata_(obj) |> get_elem("resources")
# }
#
# method(resources_, class_temporal) <- function(obj) {
#   if (clog_(obj) != "care") return(NULL)
#   endpoints_(obj) |> get_elem("resources")
# }
#
# method(resources_, class_group) <- function(obj) {
#   members_(obj) |> map(resources_)
# }

#--------- dimensions ------------
#' @autoglobal
#' @noRd
# dimensions_ <- new_generic("dimensions_", "obj", function(obj) {
#   S7_dispatch()
# })
#
# method(dimensions_, class_backend) <- function(obj) {
#   prop(obj, "dimensions")
# }
#
# method(dimensions_, class_group) <- function(obj) {
#   members_(obj) |> map(dimensions_)
# }

#' @autoglobal
#' @noRd
# rows_ <- new_generic("rows_", "obj", function(obj) {
#   S7_dispatch()
# })
#
# method(rows_, class_backend) <- function(obj) {
#   dimensions_(obj) |> prop("rows")
# }
#
# method(rows_, class_group) <- function(obj) {
#   members_(obj) |> map(rows_)
# }

#' @autoglobal
#' @noRd
# limit_ <- new_generic("limit_", "obj", function(obj) {
#   S7_dispatch()
# })
#
# method(limit_, class_backend) <- function(obj) {
#   dimensions_(obj) |> prop("limit")
# }
#
# method(limit_, class_group) <- function(obj) {
#   members_(obj) |> map(limit_)
# }

#' @autoglobal
#' @noRd
# fields_ <- new_generic("fields_", "obj", function(obj) {
#   S7_dispatch()
# })
#
# method(fields_, class_backend) <- function(obj) {
#   dimensions_(obj) |> prop("fields")
# }
#
# method(fields_, class_group) <- function(obj) {
#   members_(obj) |> map(fields_)
# }

#' @autoglobal
#' @noRd
#' field_names_ <- new_generic("field_names_", "obj", function(obj) {
#'   S7_dispatch()
#' })
#'
#' method(field_names_, class_backend) <- function(obj) {
#'   fields_(obj) |> names()
#' }
#'
#' method(field_names_, class_group) <- function(obj) {
#'   members_(obj) |> map(field_names_)
#' }
#'
#' #' @autoglobal
#' #' @noRd
#' name_fields_ <- function(x, obj) {
#'   set_names(x, field_names_(obj))
#' }

#' @autoglobal
#' @noRd
# identifier_ <- new_generic("identifier_", "obj", function(obj) {
#   S7_dispatch()
# })
#
# method(identifier_, class_list) <- function(obj) {
#   get_elem(obj, "identifier")
# }
#
# method(identifier_, class_endpoint) <- function(obj) {
#   prop(obj, "identifier")
# }
#
#
# method(identifier_, class_temporal) <- function(obj) {
#   endpoints_(obj) |> get_elem("identifier")
# }
#
# method(identifier_, class_group) <- function(obj) {
#   members_(obj) |> map(identifier_)
# }
