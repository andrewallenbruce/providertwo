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
            mtt(fileSize = parse_bytes(as.character(fileSize)),
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

# class_API
#
# x <- enrollee_API()
#
# S7::method(print, class_API) <- function(x, ...) {
#
#   ob <- props(x)
#   ob$identifier <- NULL
#   ob$resourcesAPI <- NULL
#   ob$description <- substr(ob@description, 1, 415)
#   id <- props(x@identifier)
#   re <- props(x@resourcesAPI)
#   # re_files <- as.list(re$files)
#   # re$files <- NULL
#
#   ob
#
#   glue::glue_data(
#     x@resourcesAPI@files,
#     "{.strong {.field <<name>>}} >=> <<fileSize>>",
#     .open = "<<",
#     .close = ">>")
#
#   cli::col_cyan(x@title)
#
#
#   paste(
#     glue::glue(
#       '"*" = cli::style_hyperlink("{text}", {url})',
#       text = c("Landing Page", "Data Dictionary"),
#       url = c("x@landingPage", "x@describedBy")
#       )) |>
#     rlang::parse_expr() |>
#     rlang::eval_tidy()
#
#   cli::cat_boxx(
#     c(cli::cli_text(),
#       cli::cli_bullets(
#         c("*" = cli::style_hyperlink("Landing Page", x@landingPage),
#           "*" = cli::style_hyperlink("Data Dictionary", x@describedBy))))
#   )
#
#   cli::boxx(
#     label = c(cli::cli_text(substr(x@description, 1, 415)),
#               cli::cli_bullets(
#                 c("*" = cli::style_hyperlink("Landing Page", x@landingPage),
#                   "*" = cli::style_hyperlink("Data Dictionary", x@describedBy)))),
#     border_style="round",
#     padding = 1,
#     header = cli::col_cyan(x@title))
#
#   cat(
#     "Accrual Periodicity: ", x@accrualPeriodicity, "\n",
#     "Last Modified: ", x@modified, "\n",
#     "Temporal Range: ", x@temporal, "\n",
#     "Identifier: ", x@identifier, "\n",
#     "Resources API: ", x@resourcesAPI, "\n",
#   )
# }
