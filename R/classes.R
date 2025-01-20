#' @include props.R
NULL

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
            mtt(fileSize = trimws(as.character(parse_bytes(as.character(fileSize)))),
                fileType = tools::file_ext(downloadURL)) |>
            colorder(downloadURL, pos = "end")
        }})
  )
)

#' API Class
#'
#' `class_API` object
#'
#' @param title `<chr>` Dataset title
#' @param description `<chr>` Dataset description
#' @param accrualPeriodicity `<chr>` Dataset update frequency
#' @param modified `<chr>` Date Dataset was last modified
#' @param temporal `<chr>` Date range the Current dataset covers
#' @param identifier `<S7_class>` dcat:Dataset url and nrows in dataset
#' @param accessURL `<chr>` dcat:Distribution url
#' @param resourcesAPI `<S7_class>` `data.frame` of available supplemental resources
#' @param downloadURL `<chr>` dcat:Distribution url to csv versions
#' @param describedBy `<chr>` Link to Data dictionary
#' @param landingPage `<chr>` Link to API landing page
#' @returns `<S7_class>` object
#' @autoglobal
#' @export
class_API <- new_class(
  name = "class_API",
  properties = list(
    title              = class_character,
    description        = class_character,
    accrualPeriodicity = class_character,
    modified           = class_dbl_Date,
    temporal           = class_character,
    identifier         = class_Identifier,
    accessURL          = class_character,
    resourcesAPI       = class_Resources,
    downloadURL        = class_character,
    describedBy        = class_character,
    landingPage        = class_character
  )
)
