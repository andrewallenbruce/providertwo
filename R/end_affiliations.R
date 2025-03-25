#' Provider Facility Affiliations
#'
#' Facility affiliations data publicly reported in the Provider Data Catalog.
#'
#' @section Accrual Periodicity:
#'    * `r roxy8601("R/P1M")`
#'
#' @param npi `<chr>` 10-digit Individual NPI
#' @param pac `<chr>` 10-digit PECOS Associate Control (PAC) ID
#' @param first,middle,last,suffix `<chr>` Individual provider's name(s)
#' @param facility_type `<chr>` Type of facility, one of the following:
#'
#'    * `Hospital` (`hp`)
#'    * `Long-term care hospital` (`ltch`)
#'    * `Nursing home` (`nh`)
#'    * `Inpatient rehabilitation facility` (`irf`)
#'    * `Home health agency` (`hha`)
#'    * `Skilled nursing facility` (`snf`)
#'    * `Hospice` (`hs`)
#'    * `Dialysis facility` (`df`)
#'
#' @param ccn_facility `<chr>` 6-digit CMS Certification Number (CCN) of
#'                             facility or unit within hospital where an
#'                             individual provider provides service.
#'
#' @param ccn_parent `<int>` 6-digit CMS Certification Number (CCN) of a
#'                           sub-unit's primary hospital, should the
#'                           provider provide services in said unit.
#'
#' @examples
#' affiliations(last = "CURRY", facility_type = "Home health agency")
#' affiliations(ccn_parent = "670055")
#' affiliations(ccn_facility = "370781")
#' affiliations(ccn_facility = "331302")
#' affiliations(ccn_facility = "33Z302")
#' affiliations(npi = "1043245657")
#' affiliations(last = "CURRY")
#' @autoglobal
#' @export
affiliations <- function(npi           = NULL,
                         pac           = NULL,
                         first         = NULL,
                         middle        = NULL,
                         last          = NULL,
                         suffix        = NULL,
                         facility_type = NULL,
                         ccn_facility  = NULL,
                         ccn_parent    = NULL) {

  args <- list2(
    "npi"                                        = npi,
    "ind_pac_id"                                 = pac,
    "provider_last_name"                         = last,
    "provider_first_name"                        = first,
    "provider_middle_name"                       = middle,
    "suff"                                       = suffix,
    "facility_type"                              = facility_type,
    "facility_affiliations_certification_number" = ccn_facility,
    "facility_type_certification_number"         = ccn_parent)


    # new_request(CurrentProvider(fnm(call_match()[1])))

}

#' @noRd
fnm <- function(x) {
  stringi::stri_sub(as_chr(x), to = -1)
}
