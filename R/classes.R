#' @include props.R
NULL

#' contactPoint Class
#'
#' `class_contactPoint` object
#'
#' @param type `<chr>` type
#' @param fn `<chr>` contact name
#' @param hasEmail `<chr>` contact email
#' @param hasURL `<chr>` contact url
#'
#' @returns `<S7_class>` object
#'
#' @autoglobal
#'
#' @export
class_contactPoint <- new_class(
  name       = "class_contactPoint",
  properties = list(
    type     = new_property(
      class_character,
      default = "vcard:Contact"),
    fn       = null_character,
    hasEmail = null_character,
    hasURL   = null_character
  )
)

#' publisher Class
#'
#' `class_publisher` object
#'
#' @param type `<chr>` type
#' @param name `<chr>` publisher name
#'
#' @returns `<S7_class>` object
#'
#' @autoglobal
#'
#' @export
class_publisher <- new_class(
  name       = "class_publisher",
  properties = list(
    type     = new_property(
      class_character,
      default = "org:Organization"),
    name     = new_property(
      class_character,
      default = "Centers for Medicare & Medicaid Services")
  )
)

#' Identifier Class
#'
#' `class_Identifier` object
#'
#' @param url `<chr>` Identifier url
#'
#' @returns `<S7_class>` object
#'
#' @autoglobal
#'
#' @export
class_Identifier <- new_class(
  name       = "class_Identifier",
  properties = list(
    url      = new_property(
      class  = null_character,
      setter = function(self, value) {
        self@url <- value
        self
      }),
    rows     = new_property(
      class  = null_integer,
      getter = function(self) {
        if (not_null(self@url)) {
          is_online()
          request(self@url) |>
            req_url_path_append("stats") |>
            req_perform() |>
            resp_body_json(simplifyVector = TRUE) |>
            gelm("total_rows")
        }})
    ),
    validator = function(self) {
      if (length(self@url) != 1L) "@url must be length 1"
    }
  )

#' Resources Class
#'
#' `class_Resources` object
#'
#' @param url `<chr>` resourcesAPI url
#'
#' @returns `<S7_class>` object
#'
#' @autoglobal
#'
#' @export
class_Resources <- new_class(
  name = "class_Resources",
  properties = list(
    url      = new_property(
      class  = null_character,
      setter = function(self, value) {
        self@url <- value
        self
      }),
    files    = new_property(
      class  = null_list,
      getter = function(self) {
        if (not_null(self@url)) {
          qTBL(fload(self@url, query = "/data")) |>
            mtt(fileSize = trimws(
              as.character(
                parse_bytes(
              as.character(fileSize)
            ))),
            fileType = tools::file_ext(downloadURL)) |>
            colorder(downloadURL, pos = "end")
        }})
  )
)

#' API Class
#'
#' `class_API` object
#'
#' @param type `<chr>` type
#' @param accessLevel `<chr>` Dataset access level
#' @param bureauCode `<chr>` Dataset bureau code
#' @param programCode `<chr>` Dataset program code
#' @param contactPoint `<S7_class>`
#' @param dataQuality `<lgl>` Dataset data quality
#' @param publisher `<S7_class>`
#' @param references `<chr>` Dataset references
#' @param title `<chr>` Dataset title
#' @param description `<chr>` Dataset description
#' @param accrualPeriodicity `<chr>` Dataset update frequency
#' @param issued `<chr>` Date Dataset was issued
#' @param modified `<chr>` Date Dataset was last modified
#' @param released `<chr>` Date Dataset was released
#' @param temporal `<chr>` Date range the Current dataset covers
#' @param identifier `<S7_class>` dcat:Dataset url and nrows in dataset
#' @param accessURL `<chr>` dcat:Distribution url
#' @param resourcesAPI `<S7_class>` `data.frame` of available supplemental resources
#' @param downloadURL `<chr>` dcat:Distribution url to csv versions
#' @param describedBy `<chr>` Hyperlink to Data dictionary
#' @param landingPage `<chr>` Hyperlink to API landing page
#'
#' @returns `<S7_class>` object
#'
#' @autoglobal
#'
#' @export
class_API <- new_class(
  name = "class_API",
  properties = list(
    type               = new_property(class_character, default = "dcat:Dataset"),
    accessLevel        = new_property(class_character, default = "public"),
    bureauCode         = new_property(class_character, default = "009:38"),
    programCode        = new_property(class_character, default = "009:000"),
    contactPoint       = class_contactPoint,
    dataQuality        = new_property(class_logical, default = TRUE),
    publisher          = class_publisher,
    references         = class_character,
    title              = class_character,
    description        = class_character,
    accrualPeriodicity = class_character,
    issued             = null_dbl_Date,
    modified           = null_dbl_Date,
    released           = null_dbl_Date,
    temporal           = class_character,
    identifier         = class_Identifier,
    accessURL          = class_character,
    resourcesAPI       = class_Resources,
    downloadURL        = class_character,
    describedBy        = class_character,
    landingPage        = class_character
  )
)
