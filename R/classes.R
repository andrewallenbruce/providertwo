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
class_catalog <- new_class(
  name         = "class_catalog",
  package      = NULL,
  abstract     = TRUE,
  properties   = list(
    identifier = class_character | class_data.frame
  )
)

#' @noRd
#' @autoglobal
class_care <- new_class(
  name    = "class_care",
  package = NULL,
  parent  = class_catalog
)

#' @noRd
#' @autoglobal
care_current <- new_class(
  name         = "care_current",
  package      = NULL,
  parent       = class_care,
  properties   = list(
    resources  = class_character
  )
)

#' @noRd
#' @autoglobal
care_temporal <- new_class(
  name    = "care_temporal",
  package = NULL,
  parent  = class_care
)

#' @noRd
#' @autoglobal
class_prov <- new_class(
  name    = "class_prov",
  package = NULL,
  parent  = class_catalog
)

#' @noRd
#' @autoglobal
class_caid <- new_class(
  name    = "class_caid",
  package = NULL,
  parent  = class_catalog
)

#' @noRd
#' @autoglobal
class_open <- new_class(
  name    = "class_open",
  package = NULL,
  parent  = class_catalog
)

#' @noRd
#' @autoglobal
class_hgov <- new_class(
  name    = "class_hgov",
  package = NULL,
  parent  = class_catalog
)

#' @noRd
#' @autoglobal
class_endpoint <- new_class(
  name         = "class_endpoint",
  package      = NULL,
  abstract     = TRUE,
  properties   = list(
    access     = care_current | care_temporal | class_prov | class_caid | class_open | class_hgov,
    metadata   = class_list,
    dimensions = class_dimensions
    )
  )

#' @noRd
#' @autoglobal
class_current <- new_class(
  name       = "class_current",
  package    = NULL,
  parent     = class_endpoint
)

#' @noRd
#' @autoglobal
class_temporal <- new_class(
  name       = "class_temporal",
  package    = NULL,
  parent     = class_endpoint
)

#' @noRd
#' @autoglobal
class_group <- new_class(
  name       = "class_group",
  package    = NULL,
  properties = list(
    name     = class_character,
    members  = class_list
  )
)

#' @noRd
#' @autoglobal
class_collection <- new_class(
  name       = "class_collection",
  package    = NULL,
  parent     = class_group
)

