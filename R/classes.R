#' `endpoint_current` Class
#'
#' @name endpoint_current
#'
#' @param title       `<chr>`  Endpoint title
#' @param description `<chr>`  Endpoint description
#' @param contact     `<chr>`  Endpoint contact information
#' @param modified    `<Date>` Date Endpoint data was last modified
#' @param identifier  `<chr>`  Endpoint uuid or url
#' @param download    `<chr>`  Endpoint download url
#'
#' @returns `endpoint_current` object
#' @family classes
#' @autoglobal
#' @export
endpoint_current <- new_class(
  name = "endpoint_current",
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

#' `main_resources` Class
#'
#' @name main_resources
#'
#' @param url `<chr>` default is `NA`
#'
#' @returns `main_resources` object
#' @family classes
#' @autoglobal
#' @export
main_resources <- new_class(
  name       = "main_resources",
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

#' `main_current` Class
#'
#' @name main_current
#'
#' @param title       `<chr>`  API endpoint title
#' @param description `<chr>`  API endpoint description
#' @param contact     `<chr>`  Contact information
#' @param modified    `<Date>` Date data was last modified
#' @param identifier  `<chr>`  Query url
#' @param download    `<chr>`  Download url
#'
#' @param temporal    `<chr>`  Timespan covered by data
#' @param periodicity `<chr>`  Update frequency
#' @param resources   `<chr>`  Files available for download
#' @param dictionary  `<chr>`  Link to data dictionary
#' @param site        `<chr>`  Link to landing page
#' @param references  `<chr>`  Link to references
#'
#' @returns `main_current` object
#' @family classes
#' @autoglobal
#' @export
main_current <- new_class(
  parent = endpoint_current,
  name   = "main_current",
  properties = list(
    temporal    = class_character,
    periodicity = class_character,
    resources   = main_resources,
    dictionary  = class_character,
    site        = class_character,
    references  = class_character
  )
)

#' `provider_current` Class
#'
#' @name provider_current
#'
#' @param title       `<chr>` Endpoint title
#' @param description `<chr>` Endpoint description
#' @param contact     `<chr>` Endpoint contact information
#' @param modified    `<Date>` Date Endpoint data was last modified
#' @param identifier  `<chr>` Endpoint url
#' @param download    `<chr>` Endpoint download url
#'
#' @param issued      `<Date>` Date Endpoint data was issued
#' @param released    `<Date>` Date Endpoint data was released
#' @param site        `<chr>` Hyperlink to API landing page
#'
#' @returns `provider_current` object
#' @family classes
#' @autoglobal
#' @export
provider_current <- new_class(
  parent = endpoint_current,
  name   = "provider_current",
  properties = list(
    issued = new_property(
      class_character | class_Date,
      setter = function(self, value) {
          self@issued <- as_date(value)
          self
        }),
    released = new_property(
      class_character | class_Date,
      setter = function(self, value) {
        self@released <- as_date(value)
        self
      }),
    site = class_character,
    dictionary  = new_property(
      class_character,
      getter = function(self) {
        prov_uuid_dict(self@identifier)
        })
    )
  )

#' `endpoint_temporal` Class
#'
#' @name endpoint_temporal
#'
#' @param title `<chr>` Endpoint title
#' @param description `<chr>` Endpoint description
#' @param contact `<chr>` Endpoint contact information
#' @param modified `<Date>` Date Endpoint data was last modified
#' @param endpoints `<data.frame>` Endpoints data.frame
#'
#' @returns `endpoint_temporal` object
#' @family classes
#' @autoglobal
#' @export
endpoint_temporal <- new_class(
  name = "endpoint_temporal",
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
