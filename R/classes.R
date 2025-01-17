null_numeric      <- new_union(NULL, class_numeric)
null_integer      <- new_union(NULL, class_integer)
null_character    <- new_union(NULL, class_character)
null_list         <- new_union(NULL, class_list)
null_data.frame   <- new_union(NULL, class_data.frame)
class_double_Date <- new_union(class_double, class_Date)

#' Identifier Class
#'
#' The `class_Identifier` object
#'
#' @param url `<chr>` Identifier url
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
#' The `class_Resources` object
#'
#' @param url `<chr>` ResourcesAPI url
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
      class  = null_data.frame,
      getter = function(self) {
        if (not_null(self@url)) {
          qTBL(fload(self@url, query = "/data")) |>
            mtt(fileSize = prettyunits::pretty_bytes(fileSize, "nopad"))
        }})
  )
)

#' API Class
#'
#' The `class_API` object contains validated API metadata
#'
#' @param title `<chr>` Source
#' @param description `<chr>` Source
#' @param accrualPeriodicity `<chr>` Source
#' @param modified `<chr>` Source
#' @param temporal `<chr>` Source
#' @param identifier `<class_Identifier>` Source
#' @param accessURL `<chr>` Source
#' @param resourcesAPI `<class_Resources>` Source
#' @param downloadURL `<chr>` Source
#' @param describedBy `<chr>` Source
#' @param landingPage `<chr>` Source
#' @returns valid `class_API` classed object
#'
#' @examples
#' x <- enrollapi()
#'
#' enroll <- class_API(
#'   x$title,
#'   x$description,
#'   x$accrualPeriodicity,
#'   x$modified,
#'   x$temporal,
#'   class_Identifier(x$identifier),
#'   x$accessURL,
#'   class_Resources(x$resourcesAPI),
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
class_API <- new_class(
  name = "class_API",
  properties = list(
    title              = class_character,
    description        = class_character,
    accrualPeriodicity = class_character,
    modified           = class_double_Date,
    temporal           = class_character,
    identifier         = class_Identifier,
    accessURL          = new_property(
      class  = null_character,
      setter = function(self, value) {
        self@accessURL <- value
        self
        }),
    resourcesAPI       = class_Resources,
    downloadURL        = class_character,
    describedBy        = class_character,
    landingPage        = class_character
  )
)
