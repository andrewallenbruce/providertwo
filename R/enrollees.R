#' Provider Enrollment in Medicare
#'
#' Provides access to individual and organizational enrollment level data on
#' providers actively approved to bill Medicare.
#'
#' @section accrualPeriodicity:
#'   * `R/P1Y` (Annually)
#'
#' @section Links:
#'    * [Provider Enrollment API](https://data.cms.gov/provider-characteristics/medicare-provider-supplier-enrollment/medicare-fee-for-service-public-provider-enrollment)
#'    * [Provider Enrollment Data Dictionary](https://data.cms.gov/resources/medicare-fee-for-service-public-provider-enrollment-data-dictionary)
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

  args <- rlang::list2(
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

  url <- public_filter(
    endpoint = "dataset",
    title = "Medicare Fee-For-Service  Public Provider Enrollment")[["identifier"]] |>
    httr2::request() |>
    httr2::req_url_query(!!!format_query(args), size = 5000)

  stats <- url |>
  httr2::req_url_path_append("stats") |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  # offset_sequence(stats$data$found_rows)

  resp <- url |> httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse()

  collapse::qTBL(resp[["data"]]) |>
    setNames(names(args))

}

#' Public Provider Enrollment Experiment
#'
#' @returns `<tibble>` of search results
#'
#' @examples
#' provider_enrollment()
#'
#' @autoglobal
#'
#' @export
provider_enrollment <- \() {

  if (!exists(".__public")) .__public <<- public_dataset()

  c(
    collapse::sbt(.__public[["dataset"]], sf_detect(title, "Public Provider Enrollment")),
    collapse::sbt(.__public[["api"]], sf_detect(title, "Public Provider Enrollment")),
    collapse::sbt(.__public[["csv"]], sf_detect(title, "Public Provider Enrollment"), downloadURL)
  )

}
