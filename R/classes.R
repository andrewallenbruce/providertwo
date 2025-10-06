#' @include fields_types.R
NULL

#' @noRd
#' @autoglobal
`%:=%` <- function(sym, val) {
  lhs  <- substitute(sym)
  rhs  <- substitute(val)
  stopifnot("left hand side must be a symbol" = is.symbol(lhs))
  cl   <- call('<-', lhs, rhs)

  if (is.call(rhs)) {
    cl[[3L]]$name     <- as.character(lhs)
  } else if (S7::S7_inherits(val)) {
    val@name          <- as.character(lhs)
    cl[[3L]]          <- val
  } else {
    attr(val, "name") <- as.character(lhs)
    cl[[3L]]          <- val
  }
  eval.parent(cl)
}

#' @noRd
#' @autoglobal
class_metadata %:=% S7::new_class(
  package    = NULL,
  properties = list(
    alias       = S7::class_character,
    title       = S7::class_character,
    modified    = S7::class_Date
  )
)

#' @noRd
#' @autoglobal
class_fields %:=% S7::new_class(
  package = NULL,
  properties = list(
    keys = S7::class_character | S7::class_list,
    equal = S7::new_property(S7::class_logical,
      getter = function(self) {
        if (rlang::is_bare_character(self@keys)) return(TRUE)
        collapse::allv(is.element(self@keys[-1], self@keys[1]), value = TRUE)
      }),
    constants = S7::new_property(S7::class_character | S7::class_list,
      getter = function(self) {
        if (empty(self@keys)) return(character(0L))
        if (rlang::is_bare_character(self@keys)) return(field_switch(self@keys))
        if (rlang::is_bare_list(self@keys)) purrr::map(self@keys, field_switch)
      }),
    idx = S7::new_property(S7::class_integer | S7::class_list,
      getter = function(self) {
        if (rlang::is_bare_character(self@constants)) return(cheapr::which_not_na(self@constants))
        if (rlang::is_bare_list(self@constants)) purrr::map(self@constants, cheapr::which_not_na)
      })
    )
  )

#' @noRd
#' @autoglobal
class_dimensions %:=% S7::new_class(
  package    = NULL,
  properties = list(
    limit    = S7::class_integer,
    total    = S7::class_integer,
    pages    = S7::new_property(S7::class_integer,
      getter = function(self) {
        purrr::map_int(self@total, offset, limit = self@limit)
      }),
    lite = S7::new_property(S7::class_logical,
      getter = function(self) {
        if (length(self@pages) == 1L && self@pages == 1L) TRUE else FALSE
      })),
  validator = function(self) {
    if (length(self@limit) != 1) {
      cli::cli_abort(c("x" = "{.field @limit} must be length 1"), call = NULL)
    }
    if (self@limit < 1L) {
      cli::cli_abort(c("x" = "{.field @limit} must be greater than 0"), call = NULL)
    }
  }
)

#' @noRd
#' @autoglobal
class_endpoint %:=% S7::new_class(
  package      = NULL,
  abstract     = TRUE,
  properties   = list(
    identifier = S7::class_character,
    metadata   = class_metadata,
    fields     = class_fields,
    dimensions = class_dimensions
  )
)

#' @noRd
#' @autoglobal
class_current %:=% S7::new_class(class_endpoint, package = NULL)

#' @noRd
#' @autoglobal
class_temporal %:=% S7::new_class(
  class_endpoint,
  package = NULL,
  properties = list(year = S7::class_integer)
)

#' @noRd
#' @autoglobal
care_current %:=% S7::new_class(
  class_current,
  package = NULL,
  properties = list(resources = S7::class_character)
)

#' @noRd
#' @autoglobal
care_temporal %:=% S7::new_class(
  class_temporal,
  package = NULL,
  properties = list(resources = S7::class_character)
)

#' @noRd
#' @autoglobal
class_catalog %:=% S7::new_class(
  package = NULL,
  abstract = TRUE,
  properties = list(
    access = class_current | class_temporal
  )
)

#' @noRd
#' @autoglobal
class_care %:=% S7::new_class(
  class_catalog,
  package = NULL,
  properties = list(access = care_current | care_temporal)
)

#' @noRd
#' @autoglobal
class_prov %:=% S7::new_class(
  class_catalog,
  package = NULL,
  properties = list(access = class_current)
)
#' @noRd
#' @autoglobal
class_caid %:=% S7::new_class(class_catalog, package = NULL)

#' @noRd
#' @autoglobal
class_open %:=% S7::new_class(class_catalog, package = NULL)

#' @noRd
#' @autoglobal
class_hgov %:=% S7::new_class(class_catalog, package = NULL)

#' @noRd
#' @autoglobal
class_group %:=% S7::new_class(
  package = NULL,
  properties = list(
    title = S7::class_character,
    members = S7::class_list
  )
)

#' @noRd
#' @autoglobal
class_collection %:=% S7::new_class(class_group, package = NULL)

#' @noRd
#' @autoglobal
class_response %:=% S7::new_class(
  package = NULL,
  properties = list(
    alias = S7::class_character,
    # title = S7::class_character,
    param = S7::class_character | S7::class_list,
    year = S7::class_integer,
    string = S7::class_character,
    limit = S7::class_integer,
    found = S7::class_integer,
    total = S7::class_integer,
    pages = S7::new_property(S7::class_integer,
      getter = function(self) {
        purrr::map_int(self@found, offset, limit = self@limit)
      }),
    error = S7::new_property(S7::class_logical,
      getter = function(self) {
        if (empty(self@param)) FALSE else self@found == self@total
      })
    )
  )
