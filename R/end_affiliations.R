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
#' @examples
#' affiliations(facility_type = "Home health agency")
#' affiliations(last_name = "CURRY")
#' affiliations(ccn_primary = "670055")
#' affiliations(ccn_facility = "370781")
#' affiliations(ccn_facility = "331302")
#' affiliations(ccn_facility = "33Z302")
#' affiliations(npi = "1043245657")
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
    "npi"                                        = npi,
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
    req_url_query(!!!format_query_pro(args)) |>
    perform_simple() |>
    _[["results"]] |>
    map_na_if() |>
    rnm(pro_names$affiliations) |>
    as_tbl()
}

#' Clinicians National Downloadable File
#'
#' The Doctors and Clinicians national downloadable file is organized such
#' that each line is unique at the clinician/enrollment record/group/address
#' level. Clinicians with multiple Medicare enrollment records and/or single
#' enrollments linking to multiple practice locations are listed on
#' multiple lines.
#'
#' @section Periodicity:
#'    * `r roxy8601("R/P1M")`
#'
#' @param npi `<chr>` 10-digit Individual NPI
#' @param pac `<chr>` 10-digit Individual PECOS Associate Control (PAC) ID
#' @param enid `<chr>` 15-digit Medicare Enrollment ID
#' @param first_name,middle_name,last_name,suffix `<chr>` Individual provider's name
#' @param gender Individual provider's gender
#' @param credential Individual provider's credentials
#' @param med_school Medical school attended
#' @param grad_year Year of graduation from medical school
#' @param spec_prim Primary specialty
#' @param spec_sec Secondary specialty
#' @param telehealth Telehealth services
#' @param pac_org `<chr>` 10-digit Organization PECOS Associate Control (PAC) ID
#' @param org_n_members Number of members in the organization
#' @param facility_name Facility name
#' @param add1 Address line 1
#' @param add2 Address line 2
#' @param city City
#' @param state State
#' @param zip Zip code
#' @param phone Phone number
#' @param ind_assign Individual assignment
#' @param grp_assign Group assignment
#' @param add_id Address ID
#'
#' @returns `<tibble>` of search results
#'
#' @examples
#' clinicians(last_name = "CURRY")
#' clinicians(npi = "1043245657")
#' clinicians(state = "GA")
#' @autoglobal
#' @export
clinicians <- function(npi           = NULL,
                       pac           = NULL,
                       enid          = NULL,
                       last_name     = NULL,
                       first_name    = NULL,
                       middle_name   = NULL,
                       suffix        = NULL,
                       gender        = NULL,
                       credential    = NULL,
                       med_school    = NULL,
                       grad_year     = NULL,
                       spec_prim     = NULL,
                       spec_sec      = NULL,
                       telehealth    = NULL,
                       pac_org       = NULL,
                       org_n_members = NULL,
                       facility_name = NULL,
                       add1          = NULL,
                       add2          = NULL,
                       city          = NULL,
                       state         = NULL,
                       zip           = NULL,
                       phone         = NULL,
                       ind_assign    = NULL,
                       grp_assign    = NULL,
                       add_id        = NULL) {

  args <- list2(
    "npi"                  = npi,
    "ind_pac_id"           = pac,
    "ind_enrl_id"          = enid,
    "provider_last_name"   = last_name,
    "provider_first_name"  = first_name,
    "provider_middle_name" = middle_name,
    "suff"                 = suffix,
    "gndr"                 = gender,
    "cred"                 = credential,
    "med_sch"              = med_school,
    "grd_yr"               = grad_year,
    "pri_spec"             = spec_prim,
    "sec_spec_all"         = spec_sec,
    "telehlth"             = telehealth,
    "org_pac_id"           = pac_org,
    "num_org_mem"          = org_n_members,
    "facility_name"        = facility_name,
    "adr_ln_1"             = add1,
    "adr_ln_2"             = add1,
    "citytown"             = city,
    "state"                = state,
    "zip_code"             = zip,
    "telephone_number"     = phone,
    "ind_assgn"            = ind_assign,
    "grp_assgn"            = grp_assign,
    "adrs_id"              = add_id
  )

  proMain("clinicians") |>
    new_request() |>
    req_url_query(!!!format_query_pro(args)) |>
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

#' Clinicians Utilization
#'
#' The Doctors and Clinicians utilization data file reports volume
#' information for procedures of interest on clinician profile pages and
#' in the provider data catalog (PDC) to inform patients and caregivers
#' about clinicians' experience.
#'
#' @param npi `<chr>` 10-digit Individual NPI
#' @param pac `<chr>` 10-digit PECOS Associate Control (PAC) ID
#' @param first_name,middle_name,last_name,suffix `<chr>` Individual provider's name
#' @param procedure `<chr>` Procedure category
#' @param count `<int>` Number of procedures performed
#' @param percentile `<int>` Percentile of procedures performed
#'
#' @returns `<tibble>` of search results
#'
#' @examples
#' utilization(last_name = "CURRY")
#' utilization(npi = "1043245657")
#' utilization(npi = "1003001785")
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

  args <- list2(
    "npi"                  = npi,
    "ind_pac_id"           = pac,
    "provider_last_name"   = last_name,
    "provider_first_name"  = first_name,
    "provider_middle_name" = middle_name,
    "suff"                 = suffix,
    "procedure_category"   = procedure,
    "count"                = count,
    "percentile"           = percentile
  ) |>
    format_query_pro()

  req <- proMain("utilization") |> new_request() |> req_url_query(!!!args)

  n <- req_url_query(req, count = "true", results = "false") |> perform_simple() |> _[["count"]]

  if (!n) {
    return(NULL)
  }

  if (n) {
    req_url_query(req, count = "false", results = "true") |>
      perform_simple() |>
      _[["results"]] |>
      map_na_if() |>
      rnm(pro_names$utilization) |>
      as_tbl()
  }
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
    "profile_display_indicator" = "display"
  )
)
