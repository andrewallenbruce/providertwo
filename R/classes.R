#' Identifier Class
#'
#' The `Identifier` class object contains validated API metadata
#'
#' @param url `NULL` | `class_list` API identifier url
#'
#' @autoglobal
#'
#' @export
Identifier <- new_class(
  name       = "Identifier",
  properties = list(
    url      = new_property(
      class  = NULL | class_list,
      setter = function(self, value) {
        if (not_null(value)) {
          self@url <- url_parse(value)
          self
        }}),
    totalRows = new_property(
      class  = NULL | class_integer,
      getter = function(self) {
        if (not_null(self@url)) {
          url_build(self@url) |>
            request() |>
            req_url_path_append("stats") |>
            req_perform() |>
            resp_body_json(simplifyVector = TRUE) |>
            gelm("total_rows")
        }})
    )
)

#' Resources Class
#'
#' The `Resources` class object contains validated API metadata
#'
#' @param url `NULL` | `class_character` API identifier url
#'
#' @autoglobal
#'
#' @export
Resources <- new_class(
  name       = "Resources",
  properties = list(
    url = NULL | class_character,
    files = new_property(
      class  = NULL | class_data.frame,
      getter = function(self) {
        if (not_null(self@url)) {
          qTBL(fload(self@url, query = "/data")) |>
            mtt(fileSize = prettyunits::pretty_bytes(fileSize, "nopad"))
        }})
  )
)

#' Enrollee API Class
#'
#' The `enrolleeAPI` object contains validated API metadata
#'
#' @param title `<chr>` Source
#'
#' @param description `<chr>` Source
#'
#' @param accrualPeriodicity `<chr>` Source
#'
#' @param modified `<chr>` Source
#'
#' @param temporal `<chr>` Source
#'
#' @param identifier `<Identifier>` Source
#'
#' @param accessURL `<chr>` Source
#'
#' @param resourcesAPI `<Resources>` Source
#'
#' @param downloadURL `<chr>` Source
#'
#' @param describedBy `<chr>` Source
#'
#' @param landingPage `<chr>` Source
#'
#' @returns valid `enrolleeAPI` classed object
#'
#' @examples
#' x <- enrollapi()
#'
#' enrolleeAPI
#'
#' enroll <- enrolleeAPI(
#'   x$title,
#'   x$description,
#'   x$accrualPeriodicity,
#'   x$modified,
#'   x$temporal,
#'   Identifier(x$identifier),
#'   x$accessURL,
#'   Resources(x$resourcesAPI),
#'   x$downloadURL,
#'   x$describedBy,
#'   x$landingPage
#' )
#'
#' enroll
#'
#' @autoglobal
#'
#' @export
enrolleeAPI <- new_class(
  name = "enrolleeAPI",
  properties = list(
    title              = class_character,
    description        = class_character,
    accrualPeriodicity = class_character,
    modified           = new_property(class_double | class_Date),
    temporal           = class_character,
    identifier         = new_property(class  = Identifier),
    accessURL          = new_property(class  = NULL | class_list, setter = \(self, value) { if (not_null(value)) { self@accessURL <- request(value); self }}),
    resourcesAPI       = new_property(class  = Resources),
    downloadURL        = class_character,
    describedBy        = class_character,
    landingPage        = class_character
  )
)
