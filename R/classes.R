#' @include props.R
NULL

#' ContactPoint Class
#'
#' `ContactPoint` object
#'
#' @param type `<chr>` Schema contact type; default is `vcard:Contact`
#' @param fn `<chr>` contact's full name
#' @param hasEmail `<chr>` contact's email
#' @returns `<S7_class>` dataset_contactPoint object
#' @autoglobal
#' @export
ContactPoint <- new_class(
  name       = "ContactPoint",
  properties = list(
    type     = new_property(class_character, default = "vcard:Contact"),
    fn       = null_character,
    hasEmail = null_character
  ),
  validator = function(self) {
    if (length(self@type) != 1L)     "@type must be length 1"
    if (length(self@fn) != 1L)       "@fn must be length 1"
    if (length(self@hasEmail) != 1L) "@hasEmail must be length 1"
  }
)

#' Publisher Class
#'
#' `Publisher` object
#'
#' @param type `<chr>` Schema publisher type; default is `org:Organization`
#' @param name `<chr>` publisher name; default is `Centers for Medicare & Medicaid Services`
#' @returns `<S7_class>` dataset_Publisher object
#' @autoglobal
#' @export
Publisher <- new_class(
  name = "Publisher",
  properties = list(
    type = new_property(class_character, default = "org:Organization"),
    name = new_property(class_character, default = "Centers for Medicare & Medicaid Services (CMS)")
  ),
  validator = function(self) {
    if (length(self@type) != 1L) "@type must be length 1"
    if (length(self@name) != 1L) "@name must be length 1"
  }
)

#' Identifier Class
#'
#' `Identifier` object
#'
#' @param url `<chr>` identifier url
#' @returns `<S7_class>` Identifier object
#' @examples
#' Identifier(url =
#'    paste0(
#'    "https://data.cms.gov/data-api/v1/dataset/",
#'    "2457ea29-fc82-48b0-86ec-3b0755de7515/",
#'    "data-viewer"))
#'
#' @autoglobal
#' @export
Identifier <- new_class(
  name       = "Identifier",
  properties = list(
    url      = class_character,
    rows     = new_property(
      class  = class_integer,
      getter = function(self) {
        ifelse(
          sf_detect(
            self@url,
            "provider-data"),
          nrows_provider(self@url),
          nrows_public(self@url))
        }),
    fields   = new_property(
      class  = class_character,
      getter = function(self) {
        ifelse(
          sf_detect(
            self@url,
            "provider-data"),
          fields_provider(self@url),
          fields_public(self@url))
      })
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
#' @returns `<S7_class>` dataset_Resources object
#' @examples
#' Resources(url = paste0(
#'    "https://data.cms.gov/",
#'    "data-api/v1/dataset-resources/",
#'    "7dcf9ea6-ee2f-4bf1-8b5d-39c18b0e8541"
#'    ))
#'
#' @autoglobal
#' @export
Resources <- new_class(
  name = "Resources",
  properties = list(
    url      = null_character,
    files    = new_property(
      class  = null_list,
      getter = function(self) {
        qTBL(fload(self@url, query = "/data")) |>
          mtt(fileSize = trimws(
            as_chr(parse_bytes(as_chr(fileSize)))),
              fileType = file_ext(downloadURL)) |>
          colorder(downloadURL, pos = "end")
    })
  ),
  validator = function(self) {
    if (length(self@url) != 1L) "@url must be length 1"
  }
)

#' Dataset Class
#'
#' `Dataset` object
#'
#' @param type `<chr>` Schema type; default is `dcat:Dataset`
#' @param accessLevel `<chr>` Dataset access level; default is `public`
#' @param accrualPeriodicity `<chr>` Dataset update frequency
#' @param bureauCode `<chr>` Dataset bureau code; default is `009:38`
#' @param contactPoint `<S7_class>` Dataset contact
#' @param describedBy `<chr>` Hyperlink to Data dictionary
#' @param description `<chr>` Dataset description
#' @param identifier `<S7_class>` dcat:Dataset url and nrows in dataset
#' @param keyword `<chr>` Hyperlink to API landing page
#' @param landingPage `<chr>` Hyperlink to API landing page
#' @param modified `<chr>` Date Dataset was last modified
#' @param programCode `<chr>` Dataset program code; default is `009:000`
#' @param publisher `<S7_class>` Dataset publisher
#' @param references `<chr>` Dataset references
#' @param temporal `<chr>` Date range the Current dataset covers
#' @param title `<chr>` Dataset title
#' @param resourcesAPI `<S7_class>` `data.frame` of available supplemental resources
#'
#' @returns `<S7_class>` Dataset object
#'
#' @autoglobal
#'
#' @export
Dataset <- new_class(
  name = "Dataset",
  properties = list(
    title              = class_character,
    type               = new_property(class_character, default = "dcat:Dataset"),
    accessLevel        = new_property(class_character, default = "public"),
    accrualPeriodicity = null_character,
    bureauCode         = new_property(class_character, default = "009:38"),
    contactPoint       = ContactPoint,
    describedBy        = null_character,
    description        = class_character,
    identifier         = Identifier,
    keyword            = null_character,
    landingPage        = class_character,
    modified           = null_dbl_Date,
    programCode        = new_property(class_character, default = "009:000"),
    publisher          = Publisher,
    references         = class_character,
    temporal           = null_character,
    resourcesAPI       = Resources
  )
)

#' Distribution Class
#'
#' `Distribution` object
#'
#' @param type `<chr>` Schema type; default is `dcat:Distribution`
#' @param accessLevel `<chr>` Distribution access level; default is `public`
#' @param accrualPeriodicity `<chr>` Distribution update frequency
#' @param bureauCode `<chr>` Distribution bureau code; default is `009:38`
#' @param contactPoint `<S7_class>` Distribution contact
#' @param describedBy `<chr>` Hyperlink to Data dictionary
#' @param description `<chr>` Distribution description
#' @param keyword `<chr>` Hyperlink to API landing page
#' @param landingPage `<chr>` Hyperlink to API landing page
#' @param programCode `<chr>` Distribution program code; default is `009:000`
#' @param publisher `<S7_class>` Distribution publisher
#' @param references `<chr>` Distribution references
#' @param title `<chr>` Distribution title
#' @param distributions `<S7_class>` `data.frame` of available distributions
#'
#' @returns `<S7_class>` Distribution object
#'
#' @autoglobal
#'
#' @export
Distribution <- new_class(
  name           = "Distribution",
  properties     = list(
    title              = class_character,
    type               = new_property(class_character, default = "dcat:Distribution"),
    accessLevel        = new_property(class_character, default = "public"),
    accrualPeriodicity = class_character,
    bureauCode         = new_property(class_character, default = "009:38"),
    contactPoint       = ContactPoint,
    describedBy        = class_character,
    description        = class_character,
    keyword            = class_character,
    landingPage        = class_character,
    programCode        = new_property(class_character, default = "009:000"),
    publisher          = Publisher,
    references         = class_character,
    distributions      = class_list
  )
)
