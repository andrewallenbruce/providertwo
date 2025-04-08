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
#' @param ccn_facility `<chr>` 6-digit CMS Certification Number (CCN) of
#'   facility or unit within hospital where an individual provider provides service.
#'
#' @param ccn_primary `<int>` 6-digit CMS Certification Number (CCN) of a
#'   sub-unit's primary hospital, should the provider provide services in said unit.
#'
#' @examples
#' affiliations(facility_type = "Home health agency")
#' # affiliations(last_name = "CURRY")
#' # affiliations(ccn_primary = "670055")
#' # affiliations(ccn_facility = "370781")
#' # affiliations(ccn_facility = "331302")
#' # affiliations(ccn_facility = "33Z302")
#' # affiliations(npi = "1043245657")
#'
#' @autoglobal
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
    "npi"                                        = arg_npi(npi),
    "ind_pac_id"                                 = pac,
    "provider_last_name"                         = last_name,
    "provider_first_name"                        = first_name,
    "provider_middle_name"                       = middle_name,
    "suff"                                       = suffix,
    "facility_type"                              = facility_type,
    "facility_affiliations_certification_number" = ccn_facility,
    "facility_type_certification_number"         = ccn_primary)

  proMain("affiliations") |>
    new_request() |>
    perform_simple() |>
    _[["results"]] |>
    map_na_if() |>
    rnm(pro_names$affiliations) |>
    as_tbl()
}

#' Clinicians
#'
#' The Doctors and Clinicians utilization
#' data file reports volume information for
#' procedures of interest on clinician
#' profile pages and in the provider data
#' catalog (PDC) to inform patients and
#' caregivers about clinicians' experience.
#'
#'
#' @examples
#' clinicians()
#' @autoglobal
#' @export
clinicians <- function() {
  proMain("clinicians") |>
    new_request() |>
    perform_simple() |>
    _[["results"]] |>
    map_na_if() |>
    rnm(pro_names$clinicians) |>
    mtt(address = paste(add1, add2)) |>
    slt(-add1,
        -add2,
        -sec_spec_1,
        -sec_spec_2,
        -sec_spec_3,
        -sec_spec_4) |>
    as_tbl()
}

#' PDC Utilization
#'
#' The Doctors and Clinicians utilization
#' data file reports volume information for
#' procedures of interest on clinician
#' profile pages and in the provider data
#' catalog (PDC) to inform patients and
#' caregivers about clinicians' experience.
#'
#' @param npi `<chr>` 10-digit Individual NPI
#' @param pac `<chr>` 10-digit PECOS Associate Control (PAC) ID
#' @param first_name,middle_name,last_name,suffix `<chr>` Individual provider's name
#' @param procedure `<chr>` Procedure category
#' @param count `<int>` Number of procedures performed
#' @param percentile `<int>` Percentile of procedures performed
#'
#' @examples
#' utilization()
#'
#' @autoglobal
#' @export
utilization <- function(npi         = NULL,
                        pac         = NULL,
                        last_name   = NULL,
                        first_name  = NULL,
                        middle_name = NULL,
                        suffix      = NULL,
                        procedure   = NULL,
                        count       = NULL,
                        percentile  = NULL) {
  proMain("utilization") |>
    new_request() |>
    perform_simple() |>
    _[["results"]] |>
    map_na_if() |>
    rnm(pro_names$utilization) |>
    as_tbl()
}

#' @noRd
pro_names <- list(
  affiliations = c(
    # "npi",
    # "suff",
    # "facility_type",
    "ind_pac_id"                                 = "pac",
    "provider_last_name"                         = "last_name",
    "provider_first_name"                        = "first_name",
    "provider_middle_name"                       = "middle_name",
    "facility_affiliations_certification_number" = "ccn_facility",
    "facility_type_certification_number"         = "ccn_primary"
  ),
  clinicians = c(
    # "npi",
    # "suff",
    # "cred",
    # "med_sch",
    # "sec_spec_1", X
    # "sec_spec_2", X
    # "sec_spec_3", X
    # "sec_spec_4", X
    # "facility_name",
    "ind_pac_id"           = "pac",
    "ind_enrl_id"          = "enid",
    "provider_last_name"   = "last_name",
    "provider_first_name"  = "first_name",
    "provider_middle_name" = "middle_name",
    "gndr"                 = "gender",
    "grd_yr"               = "grad_year",
    "pri_spec"             = "spec_prim",
    "sec_spec_all"         = "spec_sec",
    "telehlth"             = "telehealth",
    "org_pac_id"           = "pac_org",
    "num_org_mem"          = "org_n_memb",
    "adr_ln_1"             = "add1",
    "adr_ln_2"             = "add2",
    "ln_2_sprs"            = "add_sprs",
    "citytown"             = "city",
    "state"                = "state",
    "zip_code"             = "zip",
    "telephone_number"     = "phone",
    "ind_assgn"            = "asn_ind",
    "grp_assgn"            = "asn_grp",
    "adrs_id"              = "add_id"
  ),
  utilization = c(
    # "npi",
    # "suff",
    # "procedure_category",
    # "count",
    # "percentile",
    "ind_pac_id"                = "pac",
    "provider_last_name"        = "last_name",
    "provider_first_name"       = "first_name",
    "provider_middle_name"      = "middle_name",
    "profile_display_indicator" = "profile"
  )
)
