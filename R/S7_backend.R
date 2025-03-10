#' @title Current
#' @name Current
#' @param title `<chr>` dataset title
#' @param description `<chr>` dataset description
#' @param contact `<chr>` dataset contact
#' @param modified `<chr>` dataset modified date
#' @param identifier `<chr>` dataset identifier
#' @param download `<chr>` dataset download URL
#' @family base-classes
#' @autoglobal
#' @export
Current <- new_class(
  name = "Current",
  properties = list(
    title       = class_character,
    description = class_character,
    contact     = class_character,
    modified    = new_property(
      class_character | class_Date,
      setter = function(self, value) {
        self@modified <- as_date(value)
        self
        }),
    identifier  = class_character,
    download    = class_character
  )
)

#' @title ResourcesMain
#' @rdname Current
#' @param url `<chr>` dataset URL
#' @family main-classes
#' @autoglobal
#' @export
ResourcesMain <- new_class(
  name       = "ResourcesMain",
  properties = list(
    url      = class_character,
    files    = new_property(
      class_list,
      getter   = function(self)
        fload(self@url, query = "/data") |>
        as_df() |>
        fcompute(
          file         = name,
          size         = roundup(fileSize / 1e6),
          ext          = file_ext(downloadURL),
          downloadurl  = downloadURL
        ) |>
        roworder(ext, -size)
    )
  ),
  validator = function(self)
    if (length(self@url) != 1L)
      "must be length 1"
)

#' @title CurrentMain
#' @rdname Current
#' @param temporal `<chr>` dataset temporal
#' @param periodicity `<chr>` dataset periodicity
#' @param resources `<ResourcesMain>` dataset resources
#' @param dictionary `<chr>` dataset dictionary
#' @param site `<chr>` dataset site
#' @param references `<chr>` dataset references
#' @family main-classes
#' @autoglobal
#' @export
CurrentMain <- new_class(
  parent = Current,
  name   = "CurrentMain",
  properties = list(
    temporal    = class_character,
    periodicity = class_character,
    resources   = ResourcesMain,
    dictionary  = class_character,
    site        = class_character,
    references  = class_character
  )
)

#' @title CurrentProvider
#' @rdname Current
#' @param issued `<chr>` dataset issued date
#' @param released `<chr>` dataset released date
#' @param site `<chr>` dataset site
#' @param dictionary `<chr>` dataset dictionary
#' @family provider-classes
#' @autoglobal
#' @export
CurrentProvider <- new_class(
  parent = Current,
  name   = "CurrentProvider",
  properties = list(
    issued = new_property(
      class_character | class_Date,
      setter = function(self, value) {
          self@issued <- as_date(value)
          self }),
    released = new_property(
      class_character | class_Date,
      setter = function(self, value) {
        self@released <- as_date(value)
        self }),
    site = class_character,
    dictionary  = new_property(
      class_character,
      getter = function(self) {
        prov_uuid_dict(self@identifier) })
    )
  )

#' @title CurrentOpen
#' @rdname Current
#' @family open-classes
#' @autoglobal
#' @export
CurrentOpen <- new_class(
  parent = Current,
  name   = "CurrentOpen",
  properties = list(
    contact = new_property(
      class_character,
      default = "Open Payments (mailto:openpayments@cms.hhs.gov)"
      )
    )
  )

#' @title Temporal
#' @name Temporal
#' @param title `<chr>` dataset title
#' @param description `<chr>` dataset description
#' @param contact `<chr>` dataset contact
#' @param modified `<chr>` dataset modified date
#' @param endpoints `<list>` dataset endpoints
#' @family base-classes
#' @autoglobal
#' @export
Temporal <- new_class(
  name = "Temporal",
  properties = list(
    title       = class_character,
    description = class_character,
    contact     = class_character,
    modified    = new_property(
      class_character | class_Date,
      setter = function(self, value) {
        self@modified <- as_date(value)
        self
      }),
    endpoints = class_list
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

#' @title TemporalOpen
#' @rdname Temporal
#' @family open-classes
#' @autoglobal
#' @export
TemporalOpen <- new_class(
  parent = Temporal,
  name   = "TemporalOpen"
)
