#' @noRd
#' @autoglobal
class_fields <- S7::new_class(
  name       = "class_fields",
  package    = NULL,
  properties = list(keys = S7::class_character | S7::class_list))

#' @noRd
#' @autoglobal
class_fields2 <- S7::new_class(
  name       = "class_fields2",
  package    = NULL,
  properties = list(
    keys  = S7::class_list,
    equal = S7::new_property(
      S7::class_logical,
      getter = function(self) {
        if (length(self@keys) == 1L) return(TRUE)
        collapse::allv(is.element(self@keys[-1], self@keys[1]), value = TRUE)
      }
    )
  )
)

#' @noRd
#' @autoglobal
class_dimensions <- S7::new_class(
  name       = "class_dimensions",
  package    = NULL,
  properties = list(
    limit    = S7::class_integer,
    total    = S7::class_integer,
    pages    = S7::new_property(
      S7::class_integer,
      getter = function(self) {
        purrr::map_int(self@total, offset, limit = self@limit)
      }
    )
  ),
  validator = function(self) {
    if (length(self@limit) != 1)
      cli::cli_abort(c("x" = "{.field @limit} must be length 1"), call = NULL)
  }
)

#' @noRd
#' @autoglobal
class_endpoint <- S7::new_class(
  name         = "class_endpoint",
  package      = NULL,
  abstract     = TRUE,
  properties   = list(
    identifier = S7::class_character,
    metadata   = S7::class_list,
    fields     = class_fields | class_fields2,
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
    identifier = S7::class_character,
    year       = S7::class_integer
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
  name = "class_prov",
  package = NULL,
  parent = class_catalog,
  properties = list(access = class_current))

#' @noRd
#' @autoglobal
class_caid <- S7::new_class(
  name    = "class_caid",
  package = NULL,
  parent  = class_catalog)

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
    title    = S7::class_character,
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
class_response <- S7::new_class(
  name       = "class_response",
  package    = NULL,
  properties = list(
    alias    = S7::class_character,
    title    = S7::class_character,
    param    = S7::new_union(NULL, S7::class_character, S7::class_list),
    year     = S7::class_integer,
    string   = S7::class_character,
    limit    = S7::class_integer,
    found    = S7::class_integer,
    total    = S7::class_integer,
    pages    = S7::new_property(S7::class_integer,
      getter = function(self) purrr::map_int(self@found, offset, limit = self@limit)),
    error    = S7::new_property(S7::class_logical,
      getter = function(self) {
        if (is.null(self@param)) return(FALSE)
        self@found == self@total
      }
    )
  )
)
