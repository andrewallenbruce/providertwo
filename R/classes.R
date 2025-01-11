#' Enrollee API Class
#'
#' The `enrolleeAPI` object contains validated API metadata
#'
#' @param title `<chr>` Source
#'
#' @param description `<chr>` Source
#'
#' @param periodicity `<chr>` Source
#'
#' @param modified `<chr>` Source
#'
#' @param temporal `<chr>` Source
#'
#' @param identifier `<chr>` Source
#'
#' @param accessurl `<chr>` Source
#'
#' @param resourceapi `<chr>` Source
#'
#' @param downloadurl `<chr>` Source
#'
#' @param dictionary `<chr>` Source
#'
#' @param landingpage `<chr>` Source
#'
#' @returns valid `enrolleeAPI` classed object
#'
#' @examples
#' x <- enrollapi()
#'
#' enroll <- enrolleeAPI(
#'           x$title,
#'           x$description,
#'           x$accrualPeriodicity,
#'           x$modified,
#'           x$temporal,
#'           x$identifier,
#'           x$accessURL,
#'           x$resourcesAPI,
#'           x$downloadURL,
#'           x$describedBy,
#'           x$landingPage)
#'
#' enroll
#'
#' @autoglobal
#'
#' @export
enrolleeAPI <- new_class(
  name          = "enrolleeAPI",
  package       = "providertwo",
  properties    = list(
    title       = class_character,
    description = class_character,
    periodicity = class_character,
    modified    = new_property(class_double | class_Date),
    temporal    = class_character,
    identifier  = new_property(
      class     = NULL | class_list,
      setter    = function(self, value) {
        if (not_null(value)) {
          self@identifier <- request(value)
          self
          }}),
    accessurl   = new_property(
      class     = NULL | class_list,
      setter    = function(self, value) {
        if (not_null(value)) {
          self@accessurl <- request(value)
          self
          }}),
    resourceapi = new_property(
      class     = NULL | class_list,
      setter    = function(self, value) {
        if (not_null(value)) {
          self@resourceapi <- request(value)
          self
        }}),
    totalrows   = new_property(
      class     = NULL | class_list,
      getter    = function(self) {
        if (not_null(self@identifier)) {
          req_url_path_append(self@identifier, "stats") |>
            req_perform() |>
            resp_body_json(simplifyVector = TRUE) |>
            gelm("total_rows")
          }}),
    downloadurl = class_character,
    dictionary  = class_character,
    landingpage = class_character
  )
)
