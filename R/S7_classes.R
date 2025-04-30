#' @noRd
#' @autoglobal
class_dimensions <- new_class(
  name = "class_dimensions",
  package = NULL,
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
        set_names(self@fields)
    )
  )
)

#' @noRd
#' @autoglobal
class_metadata <- new_class(
  name = "class_metadata",
  package = NULL,
  properties = list(
    description = class_character,
    issued      = new_union(NULL, class_character, class_Date),
    modified    = new_union(class_character, class_Date),
    released    = new_union(NULL, class_character, class_Date),
    dictionary  = new_union(NULL, class_character),
    download    = new_union(NULL, class_character),
    temporal    = new_union(NULL, class_character),
    periodicity = new_union(NULL, class_character),
    site        = new_union(NULL, class_character),
    references  = new_union(NULL, class_character)
  )
)

#' @noRd
#' @autoglobal
class_endpoint <- new_class(
  name       = "class_endpoint",
  package    = NULL,
  properties = list(
    title       = class_character,
    identifier  = class_character,
    metadata    = class_metadata,
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
    members  = new_property(
      class_list,
      getter = function(self)
        map(self@members, class_endpoint) |>
        set_names(self@members)
    )
  )
)

#' @noRd
#' @autoglobal
class_temporal <- new_class(
  name       = "class_temporal",
  package    = NULL,
  properties = list(
    title       = class_character,
    metadata    = class_metadata,
    dimensions  = class_dimensions,
    endpoints   = class_list
  )
)

#' @noRd
#' @autoglobal
class_troup <- new_class(
  name       = "class_troup",
  package    = NULL,
  properties = list(
    group = class_character,
    members = new_property(
      class_list,
      getter = function(self)
        map(self@members, class_temporal) |>
        set_names(self@members)
    )
  )
)
