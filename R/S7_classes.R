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
class_backend <- new_class(
  name     = "class_backend",
  package  = NULL,
  abstract = TRUE
)

#' @noRd
#' @autoglobal
class_endpoint <- new_class(
  name       = "class_endpoint",
  package    = NULL,
  parent     = class_backend,
  properties = list(url = class_character)
)

#' @noRd
#' @autoglobal
class_temporal <- new_class(
  name       = "class_temporal",
  package    = NULL,
  parent     = class_backend,
  properties = list(urls = class_data.frame)
)

#' @noRd
#' @autoglobal
class_catalog <- new_class(
  name         = "class_catalog",
  package      = NULL,
  abstract     = TRUE,
  properties   = list(
    metadata   = class_list,
    dimensions = class_dimensions,
    identifier = class_endpoint | class_temporal
  )
)

#' @noRd
#' @autoglobal
class_care <- new_class(
  name = "class_care",
  package = NULL,
  parent = class_catalog,
  properties = list(resources = class_endpoint | class_temporal)
)

#' @noRd
#' @autoglobal
class_caid <- new_class(
  name = "class_caid",
  package = NULL,
  parent = class_catalog
)

#' @noRd
#' @autoglobal
class_prov <- new_class(
  name = "class_prov",
  package = NULL,
  parent = class_catalog
)

#' @noRd
#' @autoglobal
class_open <- new_class(
  name = "class_open",
  package = NULL,
  parent = class_catalog
)

#' @noRd
#' @autoglobal
class_hgov <- new_class(
  name = "class_hgov",
  package = NULL,
  parent = class_catalog
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
