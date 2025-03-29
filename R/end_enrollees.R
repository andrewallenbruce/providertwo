#' Medicare Provider Enrollees
#'
#' Information on a point in time snapshot of enrollment level data for
#' providers actively enrolled in Medicare.
#'
#' Individual & organizational enrollment-level data on providers actively
#' approved to bill Medicare.
#'
#' @section Periodicity:
#'    * `r roxy8601("R/P3M")`
#'
#' @section Links:
#'    * [Enrollment API](https://data.cms.gov/provider-characteristics/medicare-provider-supplier-enrollment/medicare-fee-for-service-public-provider-enrollment)
#'    * [Enrollment Data Dictionary](https://data.cms.gov/resources/medicare-fee-for-service-public-provider-enrollment-data-dictionary)
#'
#' @param npi `<chr>` 10-digit Individual NPI
#' @param pac `<chr>` 10-digit PECOS Associate Control (PAC) ID
#' @param enid `<chr>` 15-digit Medicare Enrollment ID
#' @param specialty_code `<chr>` Enrollment specialty code
#' @param specialty_desc `<chr>` Enrollment specialty description
#' @param state `<chr>` Enrollment state abbreviation
#' @param first_name,middle_name,last_name `<chr>` Individual provider's name
#' @param org_name `<chr>` Organizational provider's name
#'
#' @returns `<tibble>` of search results
#'
#' @examples
#' enrollees(enid = "I20040309000221")
#' enrollees(npi = "1417918293", specialty_code = "14-41")
#' enrollees(pac = "2860305554")
#' enrollees(state = "GA")
#' @autoglobal
#'
#' @export
enrollees <- function(npi            = NULL,
                      pac            = NULL,
                      enid           = NULL,
                      specialty_code = NULL,
                      specialty_desc = NULL,
                      state          = NULL,
                      first_name     = NULL,
                      middle_name    = NULL,
                      last_name      = NULL,
                      org_name       = NULL) {

  args <- list2(
    "NPI"                = arg_npi(npi),
    "PECOS_ASCT_CNTL_ID" = pac,
    "ENRLMT_ID"          = enid,
    "PROVIDER_TYPE_CD"   = specialty_code,
    "PROVIDER_TYPE_DESC" = specialty_desc,
    "STATE_CD"           = arg_state(state),
    "FIRST_NAME"         = first_name,
    "MDL_NAME"           = middle_name,
    "LAST_NAME"          = last_name,
    "ORG_NAME"           = org_name
  )

  x <- MainCurrent("enrollees")

  list(
    args = args,
    request = new_request(x)
  )

}

# perform_request_public(
#   url   = endpoint(public_dataset("enrollees")),
#   query = eval_bare(process_params(arg_names = fn_fmls_names(),
#                                    field_names = fields(public_dataset("enrollees")))))
