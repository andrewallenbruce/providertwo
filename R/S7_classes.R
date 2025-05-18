#' @noRd
#' @autoglobal
null_if <- function(x) {
  if (is.null(x)) return(NULL)
  if (is_na(x)) NULL else x
}

#' @noRd
#' @autoglobal
class_metadata <- function(x) {
  list(
    title       = null_if(x$title),
    description = null_if(x$description),
    modified    = null_if(x$modified),
    group       = null_if(x$group),
    issued      = null_if(x$issued),
    released    = null_if(x$released),
    temporal    = null_if(x$temporal),
    periodicity = null_if(x$periodicity),
    download    = null_if(x$download),
    resources   = null_if(x$resources),
    dictionary  = null_if(x$dictionary),
    site        = null_if(x$site),
    references  = null_if(x$references)
  ) |>
    compact()
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
      class_character,
      getter = function(self)
        as.list(self@fields) |>
        set_names(self@fields),
      setter = function(self, value) {
        self@fields <- value
        self
      }
    )
  )
)

#' @noRd
#' @autoglobal
class_endpoint <- new_class(
  name          = "class_endpoint",
  package       = NULL,
  properties    = list(
    identifier  = class_character,
    metadata    = class_list,
    dimensions  = class_dimensions
  )
)

#' @noRd
#' @autoglobal
class_temporal<- new_class(
  name          = "class_temporal",
  package       = NULL,
  properties    = list(
    metadata    = class_list,
    dimensions  = class_dimensions,
    endpoints   = class_list
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
  )
)
