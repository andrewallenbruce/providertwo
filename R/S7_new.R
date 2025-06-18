#' @noRd
#' @autoglobal
class_dimensions2 <- new_class(
  name       = "class_dimensions2",
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
class_endpoint2 <- new_class(
  name     = "class_endpoint2",
  package  = NULL,
  properties = list(
    url = class_character
  )
)

#' @noRd
#' @autoglobal
class_endpoints <- new_class(
  name     = "class_endpoints",
  package  = NULL,
  properties = list(
    urls = class_data.frame
  )
)

#' @noRd
#' @autoglobal
class_catalog <- new_class(
  name     = "class_catalog",
  package  = NULL,
  abstract = TRUE,
  properties    = list(
    metadata    = class_list,
    dimensions  = class_dimensions2,
    identifier  = class_any
  )
)

#' @noRd
#' @autoglobal
class_care <- new_class(
  name          = "class_care",
  package       = NULL,
  parent        = class_catalog
)

#' @noRd
#' @autoglobal
class_caid <- new_class(
  name          = "class_caid",
  package       = NULL,
  parent        = class_catalog
)
