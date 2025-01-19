#' Public Provider Enrollment Experiment
#'
#' @returns `<S7_class>` object
#'
#' @examplesIf FALSE
#' enrollee_API()
#'
#' @autoglobal
#'
#' @export
enrollee_API <- \() {

  if (!exists(".__public")) .__public <<- public_dataset()

  a <- c(
    as.list(
      sbt(.__public[["dataset"]], sf_detect(title, "Public Provider Enrollment"))),
    as.list(
      sbt(.__public[["api"]], sf_detect(title, "Public Provider Enrollment"))[1, 4:5]),
    as.list(
      sbt(.__public[["csv"]], sf_detect(title, "Public Provider Enrollment"), downloadURL)[1, ]))

  class_API(
    title              = a[["title"]],
    description        = substr(a[["description"]], 1, 415),
    accrualPeriodicity = a[["accrualPeriodicity"]],
    modified           = a[["modified"]],
    temporal           = a[["temporal"]],
    identifier         = class_Identifier(url = a[["identifier"]]),
    accessURL          = a[["accessURL"]],
    resourcesAPI       = class_Resources(url = a[["resourcesAPI"]]),
    downloadURL        = a[["downloadURL"]],
    describedBy        = a[["describedBy"]],
    landingPage        = a[["landingPage"]])

}

#' Medicare Provider Enrollees
#'
#' Individual & Organizational Enrollment-level Data
#' on Providers Actively Approved to Bill Medicare.
#'
#' @section Accrual Periodicity:
#'    * `r roxy8601("R/P1Y")`
#'
#' @section Links:
#'    * [Enrollment API](https://data.cms.gov/provider-characteristics/medicare-provider-supplier-enrollment/medicare-fee-for-service-public-provider-enrollment)
#'    * [Enrollment Data Dictionary](https://data.cms.gov/resources/medicare-fee-for-service-public-provider-enrollment-data-dictionary)
#'
#' @param npi `<chr>` 10-digit Individual NPI
#'
#' @param pac `<chr>` 10-digit PECOS Associate Control (PAC) ID
#'
#' @param enid `<chr>` 15-digit Medicare Enrollment ID
#'
#' @param spec_code `<chr>` Enrollment specialty code
#'
#' @param spec_desc `<chr>` Enrollment specialty description
#'
#' @param state `<chr>` Enrollment state abbreviation
#'
#' @param first,middle,last `<chr>` Individual provider's name
#'
#' @param org `<chr>` Organizational provider's name
#'
#' @param gender `<chr>` Individual provider's gender:
#'
#'    * `"F"` (Female)
#'    * `"M"` (Male)
#'    * `"9"` (Unknown/Organization)
#'
#' @returns `<tibble>` of search results
#'
#' @examples
#' enrollees(enid = "I20040309000221")
#'
#' enrollees(npi = "1417918293", spec_code = "14-41")
#'
#' enrollees(pac = "2860305554", gender = "9")
#'
#' @autoglobal
#'
#' @export
enrollees <- function(npi       = NULL,
                      pac       = NULL,
                      enid      = NULL,
                      spec_code = NULL,
                      spec_desc = NULL,
                      first     = NULL,
                      middle    = NULL,
                      last      = NULL,
                      org       = NULL,
                      state     = NULL,
                      gender    = NULL) {

  args <- list2(
    "NPI"                = npi,
    "PECOS_ASCT_CNTL_ID" = pac,
    "ENRLMT_ID"          = enid,
    "PROVIDER_TYPE_CD"   = spec_code,
    "PROVIDER_TYPE_DESC" = spec_desc,
    "STATE_CD"           = state,
    "FIRST_NAME"         = first,
    "MDL_NAME"           = middle,
    "LAST_NAME"          = last,
    "ORG_NAME"           = org,
    "GNDR_SW"            = gender)

  api <- enrollee_API()
  url <- api@identifier@url |>
    request() |>
    req_url_query(
      !!!format_query(args),
      size = 5000)

  stats <- url |>
  req_url_path_append("stats") |>
    req_perform() |>
    resp_body_json()

  resp <- req_perform(url) |>
    resp_body_string() |>
    fparse()

  cat(format(api@title), "\n")
  utils::formatUL(
    label = "==>",
    offset = 2,
    c(paste0("Periodicity: ", format(api@accrualPeriodicity)),
      paste0("Last Modified:       ", format(api@modified)))) |>
    writeLines()
  cat("\n")

  qTBL(resp[["data"]]) |>
    setNames(names(args))

}
