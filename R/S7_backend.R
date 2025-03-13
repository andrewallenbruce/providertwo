#' API Endpoint Current Object
#'
#' @param title       `<chr>` title
#' @param description `<chr>` description
#' @param contact     `<chr>` contact name and email address
#' @param modified    `<chr>` date last modified
#' @param identifier  `<chr>` uuid
#' @param rows        `<int>` number of rows
#' @param fields      `<chr>` field names
#' @param pages       `<int>` number of pages
#' @param download    `<chr>` download URL
#'
#' @returns An S7 `<Current>` object.
#'
#' @autoglobal
#' @keywords internal
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
    pages       = class_integer,
    fields      = class_character,
    download    = class_character
  )
)

#' API Endpoint Resources Object
#'
#' @param url `<chr>` resources URL
#'
#' @returns An S7 `<class_resources>` object.
#'
#' @autoglobal
#' @keywords internal
#' @export
class_resources <- new_class(
  name = "class_resources",
  properties = list(
    url = class_character,
    files = new_property(
      class_list,
      getter = function(self) {
        fload(self@url,
              query = "/data") |>
          fcompute(
            file = name,
            size = roundup(fileSize / 1e6),
            ext = file_ext(downloadURL),
            download = downloadURL) |>
          roworder(ext, -size)
      }
    )
  ),
  validator = function(self)
    if (length(self@url) != 1L)
      "must be length 1"
)

#' API Endpoint Current Object (Main)
#'
#' @inheritParams Current
#'
#' @param temporal    `<chr>` timespan endpoint covers
#' @param periodicity `<chr>` frequency of updates
#' @param resources   `<class_resources>` resources URL
#' @param dictionary  `<chr>` link to data dictionary
#' @param site        `<chr>` endpoint landing site
#' @param references  `<chr>` link to references
#'
#' @returns An S7 `<CurrentMain>` object.
#'
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

#' API Endpoint Current Object (Provider)
#'
#' @inheritParams Current
#'
#' @param issued   `<chr>` date issued
#' @param released `<chr>` date released
#' @param uuid     `<chr>` uuid
#' @param site     `<chr>` endpoint landing site
#'
#' @returns An S7 `<CurrentProvider>` object.
#'
#' @autoglobal
#' @export
CurrentProvider <- new_class(
  parent = Current,
  name = "CurrentProvider",
  properties = list(
    issued     = class_character | class_Date,
    released   = class_character | class_Date,
    uuid       = class_character,
    identifier = new_property(class_character, getter = function(self) prov_uuid_url(self@uuid)),
    site       = class_character
  )
)

#' API Endpoint Current Object (Open Payments)
#'
#' @inheritParams Current
#'
#' @param uuid `<chr>` uuid
#'
#' @returns An S7 `<CurrentOpen>` object.
#'
#' @autoglobal
#' @export
CurrentOpen <- new_class(
  parent = Current,
  name   = "CurrentOpen",
  properties = list(
    uuid       = class_character,
    identifier = new_property(class_character, getter = function(self) open_uuid_url(self@uuid))
    )
  )

#' Temporal Endpoint Object
#'
#' @param title       `<chr>` title
#' @param description `<chr>` description
#' @param contact     `<chr>` contact name and email address
#' @param rows        `<int>` number of rows
#' @param pages       `<int>` number of pages
#' @param fields      `<chr>` field names
#' @param years       `<int>` years available
#' @param endpoints   `<data.frame>` endpoints
#'
#' @returns An S7 `<Temporal>` object.
#'
#' @autoglobal
#' @export
Temporal <- new_class(
  name = "Temporal",
  properties = list(
    title       = class_character,
    description = class_character,
    contact     = class_character,
    rows        = class_integer,
    pages       = class_integer,
    fields      = class_character,
    years       = class_integer,
    endpoints   = class_list
  )
)

#' Temporal Endpoint Object (Main)
#'
#' @inheritParams Temporal
#'
#' @returns An S7 `<TemporalMain>` object.
#'
#' @autoglobal
#' @export
TemporalMain <- new_class(
  parent = Temporal,
  name   = "TemporalMain"
)

#' Temporal Group Object (Main)
#'
#' @inheritParams Temporal
#'
#' @returns An S7 `<TemporalMainGroup>` object.
#'
#' @autoglobal
#' @export
TemporalMainGroup <- new_class(
  parent = Temporal,
  name   = "TemporalMainGroup"
)

#' Temporal Endpoint Object (Open Payments)
#'
#' @inheritParams Temporal
#'
#' @returns An S7 `<TemporalOpen>` object.
#'
#' @autoglobal
#' @export
TemporalOpen <- new_class(
  parent = Temporal,
  name   = "TemporalOpen"
)

#' Temporal Group Object (Open Payments)
#'
#' @inheritParams Temporal
#'
#' @returns An S7 `<TemporalOpenGroup>` object.
#'
#' @autoglobal
#' @export
TemporalOpenGroup <- new_class(
  parent = Temporal,
  name   = "TemporalOpenGroup"
)
