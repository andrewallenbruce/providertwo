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
#' @param ccn_primary `<int>` 6-digit CMS Certification Number (CCN) of a
#'                            sub-unit's primary hospital, should the
#'                            provider provide services in said unit.
#'
#' @examples
#' affiliations(facility_type = "Home health agency")
#' # affiliations(last_name = "CURRY")
#' # affiliations(ccn_primary = "670055")
#' # affiliations(ccn_facility = "370781")
#' # affiliations(ccn_facility = "331302")
#' # affiliations(ccn_facility = "33Z302")
#' # affiliations(npi = "1043245657")
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

  # args <- list2(
  #   "npi"                                        = arg_npi(npi),
  #   "ind_pac_id"                                 = pac,
  #   "provider_last_name"                         = last_name,
  #   "provider_first_name"                        = first_name,
  #   "provider_middle_name"                       = middle_name,
  #   "suff"                                       = suffix,
  #   "facility_type"                              = facility_type,
  #   "facility_affiliations_certification_number" = ccn_facility,
  #   "facility_type_certification_number"         = ccn_primary)
  #
  # list(
  #   args = args,
  #   request = proMain("affiliations") |> new_request()
  # )

  proMain("affiliations") |>
    new_request() |>
    perform_simple() |>
    _[["results"]] |>
    map_na_if() |>
    rnm(pro_names$affiliations) |>
    as_tbl()
}

#' Clinicians
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
    slt(-add1, -add2, -sec_spec_1, -sec_spec_2, -sec_spec_3, -sec_spec_4) |>
    as_tbl()
}

#' Utilization
#' @examples
#' utilization()
#' @autoglobal
#' @export
utilization <- function() {
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
    "ind_pac_id"                                 = "pac",
    "provider_last_name"                         = "last_name",
    "provider_first_name"                        = "first_name",
    "provider_middle_name"                       = "middle_name",
    # "suff"                                       = "suffix",
    # "facility_type",
    "facility_affiliations_certification_number" = "ccn_facility",
    "facility_type_certification_number"         = "ccn_primary"
  ),
  clinicians = c(
    # "npi",
    "ind_pac_id"           = "pac",
    "ind_enrl_id"          = "enid",
    "provider_last_name"   = "last_name",
    "provider_first_name"  = "first_name",
    "provider_middle_name" = "middle_name",
    # "suff"                 = "suffix",
    "gndr"                 = "gender",
    # "cred",
    # "med_sch",
    "grd_yr"               = "grad_year",
    "pri_spec"             = "spec_prim",
    "sec_spec_all"         = "spec_sec",
    # "sec_spec_1",
    # "sec_spec_2",
    # "sec_spec_3",
    # "sec_spec_4",
    "telehlth"             = "telehealth",
    # "facility_name",
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
    "ind_pac_id"           = "pac",
    "provider_last_name"   = "last_name",
    "provider_first_name"  = "first_name",
    "provider_middle_name" = "middle_name",
    # "suff"                 = "suffix",
    # "procedure_category"   = "procedure",
    # "count",
    # "percentile",
    "profile_display_indicator" = "prof_disp"
  )
)
