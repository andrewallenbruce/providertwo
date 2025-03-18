#' Medicare Provider Enrollees
#'
#' Individual & Organizational Enrollment-level Data
#' on Providers Actively Approved to Bill Medicare.
#'
#' @section Accrual Periodicity:
#'    * `r roxy8601("R/P3M")`
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
#' @param first,middle,last `<chr>` Individual provider's name(s)
#'
#' @param org `<chr>` Organizational provider's name
#'
#' @returns `<tibble>` of search results
#'
#' @examples
#' enrollees(enid = "I20040309000221")
#'
#' enrollees(npi = "1417918293", spec_code = "14-41")
#'
#' enrollees(pac = "2860305554")
#'
#' enrollees(state = "GA")
#'
#' @autoglobal
#'
#' @export
enrollees <- function(npi       = NULL,
                      pac       = NULL,
                      enid      = NULL,
                      spec_code = NULL,
                      spec_desc = NULL,
                      state     = NULL,
                      first     = NULL,
                      middle    = NULL,
                      last      = NULL,
                      org       = NULL) {

  list2(
    "NPI" = npi,
    "PECOS_ASCT_CNTL_ID" = pac,
    "ENRLMT_ID" = enid,
    "PROVIDER_TYPE_CD" = spec_code,
    "PROVIDER_TYPE_DESC" = spec_desc,
    "STATE_CD" = state,
    "FIRST_NAME" = first,
    "MDL_NAME" = middle,
    "LAST_NAME" = last,
    "ORG_NAME" = org
  )

  # perform_request_public(
  #   url           = endpoint(public_dataset("enrollees")),
  #   query         = eval_bare(
  #     process_params(
  #       arg_names   = fn_fmls_names(),
  #       field_names = fields(
  #         public_dataset("enrollees")))
  #     )
  #   )
}
