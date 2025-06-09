#' @noRd
#' @autoglobal
null_if <- function(x) {
  if (is.null(x)) return(NULL)
  if (is_na(x)) NULL else x
}

#' @noRd
#' @autoglobal
unlist_if <- function(x) {
  if (is.list(x)) unlist(x, use.names = FALSE) else x
}

#' @noRd
#' @autoglobal
get_metadata <- function(x) {
  compact(list(
    clog        = null_if(x$clog),
    api         = null_if(x$api),
    title       = null_if(x$title),
    description = null_if(x$description),
    modified    = null_if(x$modified),
    group       = null_if(x$group),
    issued      = null_if(x$issued),
    released    = null_if(x$released),
    temporal    = null_if(x$temporal),
    periodicity = null_if(x$periodicity),
    download    = null_if(x$download),
    resources   = unlist_if(null_if(x$resources)),
    dictionary  = null_if(x$dictionary),
    site        = null_if(x$site),
    references  = null_if(x$references)
  ))
}

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
    fields = new_property(
      class_character | class_list,
      getter = function(self)
        set_names(
          new_list(
            length(self@fields),
            character(0)),
          self@fields),
      setter = function(self, value) {
        self@fields <- value
        self
      }
    )
  )
)

#' @noRd
#' @autoglobal
class_backend <- new_class(
  name     = "class_backend",
  package  = NULL,
  abstract = TRUE)

#' @noRd
#' @autoglobal
class_endpoint <- new_class(
  name          = "class_endpoint",
  package       = NULL,
  parent        = class_backend,
  properties    = list(
    metadata    = class_list,
    identifier  = class_character,
    dimensions  = class_dimensions
  )
)

#' @noRd
#' @autoglobal
class_temporal <- new_class(
  name          = "class_temporal",
  package       = NULL,
  parent        = class_backend,
  properties    = list(
    metadata    = class_list,
    endpoints   = class_list,
    dimensions  = class_dimensions
  )
)

#' @noRd
#' @autoglobal
class_group <- new_class(
  name       = "class_group",
  package    = NULL,
  properties = list(
    group    = class_character,
    members  = class_list
  ),
  validator = function(self) {
    if (!all(have_name(self@members))) "all @members must be named"
    if (!all(map_lgl(self@members, S7_inherits, class_backend))) "all @members must be a `class_backend` object"
  }
)
