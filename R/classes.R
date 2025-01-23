#' @include props.R
NULL

#' ContactPoint Class
#'
#' `ContactPoint` object
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
ContactPoint <- new_class(
  name       = "ContactPoint",
  properties = list(
    type     = new_property(
      class_character,
      default = "vcard:Contact"),
    fn       = null_character,
    hasEmail = null_character,
    hasURL   = null_character
  )
)

#' Publisher Class
#'
#' `Publisher` object
#'
#' @param type `<chr>` type
#'
#' @param name `<chr>` publisher name
#'
#' @returns `<S7_class>` object
#'
#' @autoglobal
#'
#' @export
Publisher <- new_class(
  name       = "Publisher",
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
#' @examples
#' Identifier(url =
#'    paste0(
#'    "https://data.cms.gov/data-api/v1/dataset/",
#'    "2457ea29-fc82-48b0-86ec-3b0755de7515/",
#'    "data-viewer"))
#'
#' @autoglobal
#'
#' @export
Identifier <- new_class(
  name       = "Identifier",
  properties = list(
    url      = new_property(
      class  = null_character,
      setter = function(self, value) {
        self@url <- value
        self
      }),
    request  = new_property(
      class  = null_list,
      getter = function(self) {
        if (not_null(self@url)) {
            request(self@url)
        }}),
    stats    = new_property(
      class  = null_list,
      getter = function(self) {
        if (not_null(self@url)) {
          req_url_path_append(
            self@request,
            "stats")
        }}),
    rows     = new_property(
      class  = null_integer,
      getter = function(self) {
        if (not_null(self@url)) {
          is_online()
          req_perform(self@stats) |>
            resp_body_json(simplifyVector = TRUE) |>
            gelm("total_rows")
        }}),
    fields   = new_property(
      class  = null_character,
      getter = function(self) {
        if (not_null(self@url)) {
          is_online()
            req_url_query(
              self@request,
              size   = 1,
              offset = 0) |>
            req_perform() |>
            resp_body_json(simplifyVector = TRUE) |>
            gelm("meta") |>
            gelm("headers")
        }})
    ),
    validator = function(self) {
      if (length(self@url) != 1L) "@url must be length 1"
    }
  )

#' Resources Class
#'
#' `Resources` object
#'
#' @param url `<chr>` resourcesAPI url
#'
#' @returns `<S7_class>` object
#'
#' @examples
#' Resources(url = paste0(
#'    "https://data.cms.gov/",
#'    "data-api/v1/dataset-resources/",
#'    "7dcf9ea6-ee2f-4bf1-8b5d-39c18b0e8541"
#'    ))
#'
#' @autoglobal
#'
#' @export
Resources <- new_class(
  name = "Resources",
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
              as_chr(parse_bytes(as_chr(fileSize)))),
            fileType = file_ext(downloadURL)) |>
            colorder(downloadURL, pos = "end")
        }})
  )
)

#' Dataset Class
#'
#' `Dataset` object
#'
#' @param type `<chr>` type
#' @param accessLevel `<chr>` Dataset access level
#' @param bureauCode `<chr>` Dataset bureau code
#' @param programCode `<chr>` Dataset program code
#' @param contactPoint `<S7_class>`
#' @param publisher `<S7_class>`
#' @param references `<chr>` Dataset references
#' @param title `<chr>` Dataset title
#' @param description `<chr>` Dataset description
#' @param accrualPeriodicity `<chr>` Dataset update frequency
#' @param modified `<chr>` Date Dataset was last modified
#' @param temporal `<chr>` Date range the Current dataset covers
#' @param identifier `<S7_class>` dcat:Dataset url and nrows in dataset
#' @param accessURL `<chr>` dcat:Distribution url
#' @param resourcesAPI `<S7_class>` `data.frame` of available supplemental resources
#' @param downloadURL `<chr>` dcat:Distribution url to csv versions
#' @param describedBy `<chr>` Hyperlink to Data dictionary
#' @param landingPage `<chr>` Hyperlink to API landing page
#' @param keyword `<list>` Hyperlink to API landing page
#'
#' @returns `<S7_class>` object
#'
#' @autoglobal
#'
#' @export
Dataset <- new_class(
  name = "Dataset",
  properties = list(
    type               = new_property(class_character, default = "dcat:Dataset"),
    accessLevel        = new_property(class_character, default = "public"),
    accrualPeriodicity = class_character,
    bureauCode         = new_property(class_character, default = "009:38"),
    contactPoint       = ContactPoint,
    describedBy        = class_character,
    description        = class_character,
    identifier         = Identifier,
    keyword            = class_list,
    landingPage        = class_character,
    modified           = null_dbl_Date,
    programCode        = new_property(class_character, default = "009:000"),
    publisher          = Publisher,
    references         = class_character,
    temporal           = class_character,
    title              = class_character,
    accessURL          = class_character,
    resourcesAPI       = Resources,
    downloadURL        = class_character
  )
)
