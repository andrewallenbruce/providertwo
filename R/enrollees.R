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
#' @param gender `<chr>` Individual provider's gender:
#'
#'    * `"F"` (Female)
#'    * `"M"` (Male)
#'    * `"9"` (Unknown/Organization)
#'
#' @returns `<tibble>` of search results
#'
#' @examplesIf rlang::is_interactive()
#' enrollees(enid = "I20040309000221")
#'
#' enrollees(npi = "1417918293", spec_code = "14-41")
#'
#' enrollees(pac = "2860305554", gender = "9")
#'
#' enrollees(state = "GA", gender = "9")
#'
#' enrollees(state = "GA", gender = "F")
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
                      org       = NULL,
                      gender    = NULL) {

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
