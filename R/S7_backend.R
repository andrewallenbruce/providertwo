#' @title Current
#' @name Current
#' @param title `<chr>` title
#' @param description `<chr>` description
#' @param contact `<chr>` contact
#' @param modified `<chr>` date last modified
#' @param identifier `<chr>` uuid
#' @param rows `<int>` number of rows
#' @param fields `<chr>` field names
#' @param pages `<int>` number of pages
#' @param download `<chr>` download URL
#' @family classes
#' @autoglobal
#' @export
Current <- new_class(
  name          = "Current",
  properties    = list(
    title       = class_character,
    description = class_character,
    contact     = class_character,
    modified    = class_character | class_Date,
    identifier  = class_character,
    rows        = class_integer,
    fields      = class_character,
    pages       = class_integer,
    download    = class_character
  )
)

#' @title class_resources
#' @param url `<chr>` dataset URL
#' @family classes
#' @autoglobal
#' @export
class_resources <- new_class(
  name = "class_resources",
  properties = list(
    url = class_character,
    files = new_property(
      class_list,
      getter = function(self) {
        fload(self@url, query = "/data") |>
          fcompute(
            file = name,
            size = roundup(fileSize / 1e6),
            ext = file_ext(downloadURL),
            download = downloadURL
          ) |>
          roworder(ext, -size)
      }
    )
  ),
  validator = function(self)
    if (length(self@url) != 1L)
      "must be length 1"
)

#' @title CurrentMain
#' @rdname Current
#' @param temporal `<chr>` temporal
#' @param periodicity `<chr>` periodicity
#' @param resources `<chr>` resources URL
#' @param dictionary `<chr>` dictionary
#' @param site `<chr>` landing site
#' @param references `<chr>` references link
#' @family classes
#' @autoglobal
#' @export
CurrentMain <- new_class(
  parent = Current,
  name   = "CurrentMain",
  properties = list(
    temporal    = class_character,
    periodicity = class_character,
    resources   = class_resources,
    dictionary  = class_character,
    site        = class_character,
    references  = class_character
  )
)

#' @title CurrentProvider
#' @rdname Current
#' @param issued `<chr>` date issued
#' @param released `<chr>` date released
#' @param uuid `<chr>` uuid
#' @param site `<chr>` landing site
#' @family classes
#' @autoglobal
#' @export
CurrentProvider <- new_class(
  parent = Current,
  name = "CurrentProvider",
  properties = list(
    issued = class_character | class_Date,
    released = class_character | class_Date,
    uuid = class_character,
    identifier = new_property(class_character, getter = function(self) prov_uuid_url(self@uuid)),
    site = class_character
  )
)

#' @title CurrentOpen
#' @rdname Current
#' @param uuid `<chr>` uuid
#' @family classes
#' @autoglobal
#' @export
CurrentOpen <- new_class(
  parent = Current,
  name   = "CurrentOpen",
  properties = list(
    uuid = class_character,
    identifier = new_property(class_character, getter = function(self) open_uuid_url(self@uuid))
    )
  )

#' @title Temporal
#' @name Temporal
#' @param title `<chr>` dataset title
#' @param description `<chr>` dataset description
#' @param contact `<chr>` dataset contact
#' @param rows `<int>` number of rows
#' @param fields `<chr>` field names
#' @param pages `<int>` number of pages
#' @param years `<int>` years available
#' @param endpoints `<data.frame>` endpoints
#' @family base-classes
#' @autoglobal
#' @export
Temporal <- new_class(
  name = "Temporal",
  properties = list(
    title       = class_character,
    description = class_character,
    contact     = class_character,
    rows        = class_integer,
    fields      = class_character,
    pages       = class_integer,
    years       = class_integer,
    endpoints   = class_list
  )
)

#' @title TemporalMain
#' @rdname Temporal
#' @family main-classes
#' @autoglobal
#' @export
TemporalMain <- new_class(
  parent = Temporal,
  name   = "TemporalMain"
)

#' @title TemporalMainGroup
#' @rdname Temporal
#' @family main-classes
#' @autoglobal
#' @export
TemporalMainGroup <- new_class(
  parent = Temporal,
  name   = "TemporalMainGroup"
)

#' @title TemporalOpen
#' @rdname Temporal
#' @family open-classes
#' @autoglobal
#' @export
TemporalOpen <- new_class(
  parent = Temporal,
  name   = "TemporalOpen"
)

#' @title TemporalOpenGroup
#' @rdname Temporal
#' @family open-classes
#' @autoglobal
#' @export
TemporalOpenGroup <- new_class(
  parent = Temporal,
  name   = "TemporalOpenGroup"
)
