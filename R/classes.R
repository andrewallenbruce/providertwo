#' @noRd
#' @autoglobal
class_dimensions <- new_class(
  name       = "class_dimensions",
  package    = NULL,
  properties = list(
    limit = class_integer,
    rows  = class_integer,
    pages = new_property(
      class_integer,
      getter = function(self)
        offset_size(self@rows, self@limit)
    ),
    fields = class_list
  )
)

#' @noRd
#' @autoglobal
class_current <- new_class(
  name       = "class_current",
  package    = NULL,
  parent     = class_character
)

#' @noRd
#' @autoglobal
class_temporal <- new_class(
  name       = "class_temporal",
  package    = NULL,
  parent     = class_list
)

#' @noRd
#' @autoglobal
class_endpoint <- new_class(
  name         = "class_endpoint",
  package      = NULL,
  abstract     = TRUE,
  properties   = list(
    metadata   = class_list,
    dimensions = class_dimensions,
    identifier = class_current | class_temporal
  )
)

#' @noRd
#' @autoglobal
class_care <- new_class(
  name = "class_care",
  package = NULL,
  parent = class_endpoint,
  properties = list(
    resources = class_current | class_temporal
  )
)

#' @noRd
#' @autoglobal
class_prov <- new_class(
  name = "class_prov",
  package = NULL,
  parent = class_endpoint,
  properties = list(
    identifier = class_current
  )
)

#' @noRd
#' @autoglobal
class_caid <- new_class(
  name = "class_caid",
  package = NULL,
  parent = class_endpoint
)

#' @noRd
#' @autoglobal
class_open <- new_class(
  name = "class_open",
  package = NULL,
  parent = class_endpoint
)

#' @noRd
#' @autoglobal
class_hgov <- new_class(
  name = "class_hgov",
  package = NULL,
  parent = class_endpoint
)

#' @noRd
#' @autoglobal
class_group <- new_class(
  name       = "class_group",
  package    = NULL,
  properties = list(
    name    = class_character,
    members = class_list
  ) #,
  # validator = function(self) {
  #   if (!all(have_name(self@members))) "all @members must be named"
  #   if (!all(map_lgl(self@members, S7_inherits, class_backend))) "all @members must be a `class_backend` object"
  # }
)

#' @noRd
#' @autoglobal
class_collection <- new_class(
  name       = "class_collection",
  package    = NULL,
  parent     = class_group
)
