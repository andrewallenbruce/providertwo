#' @noRd
#' @autoglobal
class_fields <- S7::new_class(
  name       = "class_fields",
  package    = NULL,
  properties = list(
    keys     = S7::new_property(
      S7::class_character,
      setter = function(self, value) {
        self@keys <- value
        self
      },
      getter = function(self) kit::psort(self@keys, nThread = 4L))
  )
)

#' @noRd
#' @autoglobal
class_dimensions <- S7::new_class(
  name       = "class_dimensions",
  package    = NULL,
  properties = list(
    limit    = S7::class_integer,
    rows     = S7::class_integer,
    pages    = S7::new_property(
      S7::class_integer,
      getter = function(self) offset_size(self@rows, self@limit))
  )
)

#' @noRd
#' @autoglobal
class_endpoint <- S7::new_class(
  name         = "class_endpoint",
  package      = NULL,
  abstract     = TRUE,
  properties   = list(
    identifier = S7::class_character | S7::class_data.frame,
    metadata   = S7::class_list,
    fields     = class_fields,
    dimensions = class_dimensions
  )
)

#' @noRd
#' @autoglobal
class_current <- S7::new_class(
  name         = "class_current",
  package      = NULL,
  parent       = class_endpoint,
  properties   = list(
    identifier = S7::class_character
  )
)

#' @noRd
#' @autoglobal
class_temporal <- S7::new_class(
  name         = "class_temporal",
  package      = NULL,
  parent       = class_endpoint,
  properties   = list(
    identifier = S7::class_data.frame
  )
)

#' @noRd
#' @autoglobal
care_current <- S7::new_class(
  name         = "care_current",
  package      = NULL,
  parent       = class_current
)

#' @noRd
#' @autoglobal
care_temporal <- S7::new_class(
  name         = "care_temporal",
  package      = NULL,
  parent       = class_temporal
)

#' @noRd
#' @autoglobal
class_catalog <- S7::new_class(
  name         = "class_catalog",
  package      = NULL,
  abstract     = TRUE,
  properties   = list(
    access     = class_current | class_temporal
  )
)

#' @noRd
#' @autoglobal
class_care <- S7::new_class(
  name         = "class_care",
  package      = NULL,
  parent       = class_catalog,
  properties   = list(
    access     = care_current | care_temporal
  )
)

#' @noRd
#' @autoglobal
class_prov <- S7::new_class(
  name       = "class_prov",
  package    = NULL,
  parent     = class_catalog,
  properties = list(
    access   = class_current
  )
)

#' @noRd
#' @autoglobal
class_caid <- S7::new_class(
  name    = "class_caid",
  package = NULL,
  parent  = class_catalog
)

#' @noRd
#' @autoglobal
class_open <- S7::new_class(
  name    = "class_open",
  package = NULL,
  parent  = class_catalog
)

#' @noRd
#' @autoglobal
class_hgov <- S7::new_class(
  name    = "class_hgov",
  package = NULL,
  parent  = class_catalog
)

#' @noRd
#' @autoglobal
class_group <- S7::new_class(
  name       = "class_group",
  package    = NULL,
  properties = list(
    name     = S7::class_character, # title
    members  = S7::class_list
  )
)

#' @noRd
#' @autoglobal
class_collection <- S7::new_class(
  name       = "class_collection",
  package    = NULL,
  parent     = class_group
)

#' @noRd
#' @autoglobal
class_query <- S7::new_class(
  name       = "class_query",
  package    = NULL,
  properties = list(
    input    = S7::new_property(
      S7::class_list,
      setter = function(self, value) {
        self@input <- value
        self
      }
    ),
    params = class_list
  )
)

#' @noRd
#' @autoglobal
class_modifier <- S7::new_class(
  name       = "class_modifier",
  package    = NULL,
  properties = list(
    operator = S7::new_property(S7::class_character, default = "="),
    value    = S7::class_character | S7::class_numeric
  ),
  validator = function(self) {
    if (length(self@operator) != 1) "@operator must be length 1"
  }
)

#' @noRd
#' @autoglobal
class_results <- S7::new_class(
  name       = "class_results",
  package    = NULL,
  properties = list(
    title    = S7::class_character,
    params   = S7::new_union(NULL, S7::class_character, S7::class_list),
    base     = S7::class_character | S7::class_list,
    limit    = S7::class_integer,
    found    = S7::class_integer,
    total    = S7::class_integer,
    pages    = S7::new_property(
      S7::class_integer,
      getter = function(self) {
        purrr::map_int(self@found, function(x)
          offset_size(n = x, self@limit))
      }
    ),
    problem  = S7::new_property(
      S7::new_union(NULL, S7::class_logical),
      getter = function(self) {
        if (is.null(self@params)) return(NULL)
        self@found == self@total
      }
    )
  )
)
