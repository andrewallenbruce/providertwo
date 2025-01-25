#' @include props.R
NULL

#' dataset_contactPoint Class
#'
#' `dataset_contactPoint` object
#'
#' @param type `<chr>` Schema contact type; default is `vcard:Contact`
#' @param fn `<chr>` contact's full name
#' @param hasEmail `<chr>` contact's email
#' @param hasURL `<chr>` contact's url
#' @returns `<S7_class>` dataset_contactPoint object
#' @autoglobal
#' @export
dataset_contactPoint <- new_class(
  name       = "dataset_contactPoint",
  properties = list(
    type     = new_property(class_character, default = "vcard:Contact"),
    fn       = null_character,
    hasEmail = null_character,
    hasURL   = null_character
  ),
  validator = function(self) {
    if (length(self@type) != 1L)     "@type must be length 1"
    if (length(self@fn) != 1L)       "@fn must be length 1"
    if (length(self@hasEmail) != 1L) "@hasEmail must be length 1"
  }
)

#' dataset_Publisher Class
#'
#' `dataset_Publisher` object
#'
#' @param type `<chr>` Schema publisher type; default is `org:Organization`
#' @param name `<chr>` publisher name; default is `Centers for Medicare & Medicaid Services`
#' @returns `<S7_class>` dataset_Publisher object
#' @autoglobal
#' @export
dataset_Publisher <- new_class(
  name = "dataset_Publisher",
  properties = list(
    type = new_property(class_character, default = "org:Organization"),
    name = new_property(class_character, default = "Centers for Medicare & Medicaid Services (CMS)")
  ),
  validator = function(self) {
    if (length(self@type) != 1L) "@type must be length 1"
    if (length(self@name) != 1L) "@name must be length 1"
  }
)

#' public_Identifier Class
#'
#' `public_Identifier` object
#'
#' @param url `<chr>` identifier url
#' @returns `<S7_class>` public_Identifier object
#' @examples
#' public_Identifier(url =
#'    paste0(
#'    "https://data.cms.gov/data-api/v1/dataset/",
#'    "2457ea29-fc82-48b0-86ec-3b0755de7515/",
#'    "data-viewer"))
#'
#' @autoglobal
#' @export
public_Identifier <- new_class(
  name       = "public_Identifier",
  properties = list(
    url      = null_character,
    request  = new_property(
      class  = null_list,
      getter = function(self) request(self@url)),
    rows     = new_property(
      class  = null_integer,
      getter = function(self) {
        if (is_online()) {
          req_url_path_append(self@request, "stats") |>
            req_perform() |>
            resp_body_json(simplifyVector = TRUE) |>
            gelm("total_rows")
        }}),
    fields   = new_property(
      class  = null_character,
      getter = function(self) {
        if (is_online()) {
            req_url_query(self@request, size = 1, offset = 0) |>
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

#' provider_Identifier Class
#'
#' `provider_Identifier` object
#'
#' @param id `<chr>` identifier
#' @returns `<S7_class>` provider_Identifier object
#' @examples
#' provider_Identifier(id = "mj5m-pzi6")
#'
#' @autoglobal
#' @export
provider_Identifier <- new_class(
  name       = "provider_Identifier",
  properties = list(
    id       = null_character,
    request  = new_property(
      class  = null_list,
      getter = function(self) {
        request(paste0(
          "https://data.cms.gov/",
          "provider-data/api/1/",
          "datastore/query/",
          self@id,
          "/0"))
        }),
    rows     = new_property(
      class  = null_integer,
      getter = function(self) {
        if (is_online()) {
          req_url_query(self@request, limit = 1, offset = 0) |>
            req_perform() |>
            resp_body_json(simplifyVector = TRUE) |>
            gelm("count") |>
            gelm("count")
        }}),
    fields   = new_property(
      class  = null_character,
      getter = function(self) {
        if (is_online()) {
          req_url_query(self@request, limit = 1, offset = 0) |>
            req_perform() |>
            resp_body_json(simplifyVector = TRUE) |>
            gelm("query") |>
            gelm("properties")
        }})
  ),
  validator = function(self) {
    if (length(self@id) != 1L) "@id must be length 1"
  }
)

#' dataset_Resources Class
#'
#' `dataset_Resources` object
#'
#' @param url `<chr>` resourcesAPI url
#' @returns `<S7_class>` dataset_Resources object
#' @examples
#' dataset_Resources(url = paste0(
#'    "https://data.cms.gov/",
#'    "data-api/v1/dataset-resources/",
#'    "7dcf9ea6-ee2f-4bf1-8b5d-39c18b0e8541"
#'    ))
#'
#' @autoglobal
#' @export
dataset_Resources <- new_class(
  name = "dataset_Resources",
  properties = list(
    url      = null_character,
    files    = new_property(
      class  = null_list,
      getter = function(self) {
        if (is_online()) {
          qTBL(fload(self@url, query = "/data")) |>
            mtt(fileSize = trimws(as_chr(parse_bytes(as_chr(fileSize)))),
                fileType = file_ext(downloadURL)) |>
            colorder(downloadURL, pos = "end")
        }})
  ),
  validator = function(self) {
    if (length(self@url) != 1L) paste0("@url must be length 1, not ", length(self@url))
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
    contactPoint       = dataset_contactPoint,
    describedBy        = null_character,
    description        = class_character,
    identifier         = new_property(public_Identifier | provider_Identifier),
    keyword            = null_character,
    landingPage        = class_character,
    modified           = null_dbl_Date,
    programCode        = new_property(class_character, default = "009:000"),
    publisher          = dataset_Publisher,
    references         = class_character,
    temporal           = null_character,
    resourcesAPI       = dataset_Resources
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
    contactPoint       = dataset_contactPoint,
    describedBy        = class_character,
    description        = class_character,
    keyword            = class_character,
    landingPage        = class_character,
    programCode        = new_property(class_character, default = "009:000"),
    publisher          = dataset_Publisher,
    references         = class_character,
    distributions      = class_list
  )
)
