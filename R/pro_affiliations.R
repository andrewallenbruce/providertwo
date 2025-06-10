#' Provider Facility Affiliations
#'
#' Facility affiliations data publicly reported in the Provider Data Catalog.
#'
#' @section Periodicity:
#'    * `r roxy8601("R/P1M")`
#'
#' @param npi `<chr>` 10-digit Individual NPI
#' @param pac `<chr>` 10-digit PECOS Associate Control (PAC) ID
#' @param first_name,middle_name,last_name,suffix `<chr>` Individual provider's name
#'
#' @param facility_type `<chr>` Type of facility, one of the following:
#'
#'    * `"Hospital"`
#'    * `"Long-term care hospital"`
#'    * `"Nursing home"`
#'    * `"Inpatient rehabilitation facility"`
#'    * `"Home health agency"`
#'    * `"Skilled nursing facility"`
#'    * `"Hospice"`
#'    * `"Dialysis facility"`
#'
#' @param ccn_facility `<chr>` 6-digit CMS Certification Number (CCN)
#'   of a facility or unit within hospital where an individual provider
#'   provides service.
#'
#' @param ccn_primary `<int>` 6-digit CMS Certification Number (CCN)
#'   of a sub-unit's primary hospital, should the provider provide
#'   services in said unit.
#'
#' @returns `<tibble>` of search results
#'
#' @examplesIf interactive()
#' affiliations(facility_type = "Home health agency")
#' affiliations(last_name = "CURRY")
#' affiliations(ccn_primary = "670055")
#' affiliations(ccn_facility = "370781")
#' affiliations(ccn_facility = "331302")
#' affiliations(ccn_facility = "33Z302")
#' affiliations(npi = "1043245657")
#' @autoglobal
#' @rdname provider
#' @export
affiliations <- function(npi           = NULL,
                         pac           = NULL,
                         last_name     = NULL,
                         first_name    = NULL,
                         middle_name   = NULL,
                         suffix        = NULL,
                         facility_type = NULL,
                         ccn_facility  = NULL,
                         ccn_primary   = NULL) {

  args <- list2(
    "npi"                                        = npi,
    "ind_pac_id"                                 = pac,
    "provider_last_name"                         = last_name,
    "provider_first_name"                        = first_name,
    "provider_middle_name"                       = middle_name,
    "suff"                                       = suffix,
    "facility_type"                              = facility_type,
    "facility_affiliations_certification_number" = ccn_facility,
    "facility_type_certification_number"         = ccn_primary)


  x <- prov_endpoint("pdc_affiliations")

  x <- base_request(x) |>
    req_url_query(!!!format_query_pro(args)) |>
    perform_simple() |>
    _[["results"]] |>
    map_na_if() |>
    rnm(pro_names("affiliations")) |>
    as_fibble()
  x
}
