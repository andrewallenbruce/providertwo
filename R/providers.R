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
#'
#' @param npi `<chr>` 10-digit Individual NPI
#' @param pac `<chr>` 10-digit PECOS Associate Control (PAC) ID
#' @param enid `<chr>` 15-digit Medicare Enrollment ID
# @param specialty `<chr>` Enrollment specialty code __OR__ description
#' @param state `<chr>` Enrollment state abbreviation
#' @param first,middle,last `<chr>` Individual provider's name
#' @param org `<chr>` Organizational provider's name
#' @param gender `<chr>` Individual provider's gender:
#'
#'    * `"F"` (Female)
#'    * `"M"` (Male)
#'    * `"9"` (Unknown/Organization)
#'
#' @return `<tibble>` of search results
#'
#' @examples
#' providers2(enid = "I20040309000221")
#'
#' # providers2(npi = "1417918293", specialty = "14-41")
#'
#' providers2(pac = "2860305554", gender = "9")
#'
#' @autoglobal
#'
#' @export
providers2 <- function(npi       = NULL,
                       pac       = NULL,
                       enid      = NULL,
                       # specialty = NULL,
                       first     = NULL,
                       middle    = NULL,
                       last      = NULL,
                       org       = NULL,
                       state     = NULL,
                       gender    = NULL) {

  rlang::list2(
    "NPI"                = npi,
    "PECOS_ASCT_CNTL_ID" = pac,
    "ENRLMT_ID"          = enid,
    # "PROVIDER_TYPE_CD"   = specialty_code,
    # "PROVIDER_TYPE_DESC" = specialty_description,
    "STATE_CD"           = state,
    "FIRST_NAME"         = first,
    "MDL_NAME"           = middle,
    "LAST_NAME"          = last,
    "ORG_NAME"           = org,
    "GNDR_SW"            = gender)
}
