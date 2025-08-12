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
      cli::cli_abort(c("x" = "{.field @limit} must be length 1"), call. = FALSE)
  }
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
    title    = S7::class_character,
    members  = S7::class_list #,
    # validator = function(self) {
    #   if (any(purrr::map_lgl(self@members, \(x) ! S7::S7_inherits(x, class_catalog))))
    #     cli::cli_abort(c("x" = "{.field @members} must be {.cls class_catalog}"),
    #                    call. = FALSE)
    # }
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
    params    = S7::new_property(
      S7::class_list,
      setter = function(self, value) {
        self@params <- value
        self
      }
    )
  )
)
# # 2 Group Conjunctions - Index ([2]) ids the group
# "filter[g1][group][conjunction]=OR"
# "filter[g2][group][conjunction]=AND"
#
# # Add memberOf to params in group
# "filter[1][condition][memberOf]=g2"
#' @noRd
#' @autoglobal
query_group <- S7::new_class(
  name       = "query_group",
  package    = NULL,
  parent     = class_query,
  properties = list(conjunction = S7::class_character),
  validator = function(self) {
    if (length(self@conjunction) != 1)
      cli::cli_abort(c("x" = "{.field @conjunction} must be length 1"), call. = FALSE)
    if (!self@conjunction %in% c("OR", "AND"))
      cli::cli_abort(c("x" = "{.field @conjunction} must be one of {.val AND} or {.val OR}"), call. = FALSE)
  }
)

#' @noRd
#' @autoglobal
class_modifier <- S7::new_class(
  name       = "class_modifier",
  package    = NULL,
  properties = list(
    operator = S7::new_property(S7::class_character, default = "="),
    value    = S7::class_character | S7::class_numeric),
  validator  = function(self) {
    if (length(self@operator) != 1)
      cli::cli_abort(c("x" = "{.field @operator} must be length 1"), call. = FALSE)
  }
)

#' @noRd
#' @autoglobal
class_response <- S7::new_class(
  name       = "class_response",
  package    = NULL,
  properties = list(
    alias    = S7::class_character,
    title    = S7::class_character,
    params   = S7::new_union(NULL, S7::class_character),
    base     = S7::class_character | S7::class_list,
    limit    = S7::class_integer,
    found    = S7::class_integer,
    total    = S7::class_integer,
    pages    = S7::new_property(S7::class_integer,
      getter = function(self) purrr::map_int(self@found, offset, limit = self@limit)),
    error    = S7::new_property(S7::new_union(NULL, S7::class_logical),
      getter = function(self) {
        if (is.null(self@params)) return(NULL)
        self@found == self@total
      }
    )
  )
)
