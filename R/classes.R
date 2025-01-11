#' Enrollee API Class
#'
#' The `enrolleeAPI` object contains validated API metadata
#'
#' @param title `<chr>` Source
#' @param description `<chr>` Source
#' @param periodicity `<chr>` Source
#' @param modified `<chr>` Source
#' @param temporal `<chr>` Source
#' @param identifier `<chr>` Source
#' @param accessurl `<chr>` Source
#' @param resourceapi `<chr>` Source
#' @param downloadurl `<chr>` Source
#' @param dictionary `<chr>` Source
#' @param landingpage `<chr>` Source
#'
#' @returns valid `enrolleeAPI` classed object
#'
#' @examplesIf FALSE
#'
#' if (!exists(".__public")) .__public <<- public_dataset()
#'
#' x <- as.list(
#'      collapse::sbt(.__public[["dataset"]],
#'      sf_detect(title, "Public Provider Enrollment")))
#'
#' y <- as.list(
#'      collapse::sbt(.__public[["api"]],
#'      sf_detect(title, "Public Provider Enrollment"))[1, 4:5])
#'
#' z <- collapse::sbt(.__public[["csv"]],
#'      sf_detect(title, "Public Provider Enrollment"),
#'      downloadURL)[1,][[1]]
#'
#' enrolleeAPI(
#'   title       = x$title,
#'   description = x$description,
#'   periodicity = x$accrualPeriodicity,
#'   modified    = x$modified,
#'   temporal    = x$temporal,
#'   identifier  = x$identifier,
#'   accessurl   = y$accessURL,
#'   resourceapi = y$resourcesAPI,
#'   downloadurl = z,
#'   dictionary  = x$describedBy,
#'   landingpage = x$landingPage)
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
    modified    = new_property(class_Date | class_numeric),
    temporal    = new_property(class_Date | class_numeric),
    identifier  = new_property(
      class     = class_list | NULL,
      setter    = \(self, value) {
      self@identifier <- httr2::request(value)
      self
    }),
    accessurl   = new_property(
      class     = class_list | NULL,
      setter    = \(self, value) {
      self@accessurl <- httr2::request(value)
      self
    }),
    resourceapi = new_property(
      class     = class_list | NULL,
      setter    = \(self, value) {
      self@resourceapi <- httr2::request(value)
      self
    }),
    totalrows   = new_property(
      class     = class_integer | NULL,
      getter    = \(self) {
      httr2::req_url_path_append(self@identifier, "stats") |>
        httr2::req_perform() |>
        httr2::resp_body_json(simplifyVector = TRUE) |>
        gelm("total_rows")
    }),
    downloadurl = class_character,
    dictionary  = class_character,
    landingpage = class_character
  )
)
