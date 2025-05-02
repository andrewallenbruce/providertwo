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

  pro_endpoint("PDC_affiliations") |>
    base_request() |>
    req_url_query(!!!format_query_pro(args)) |>
    perform_simple() |>
    _[["results"]] |>
    map_na_if() |>
    rnm(pro_names("affiliations")) |>
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

  pro_endpoint("PDC_clinicians") |>
    base_request() |>
    req_url_query(!!!format_query_pro(args)) |>
    perform_simple() |>
    _[["results"]] |>
    map_na_if() |>
    mtt(
      address = make_address(adr_ln_1, adr_ln_2),
      grd_yr = as.integer(grd_yr),
      num_org_mem = as.integer(num_org_mem),
      telehlth = val_match(telehlth, "N" ~ 0L, "Y" ~ 1L),
      ind_assgn = val_match(ind_assgn, "N" ~ 0L, "Y" ~ 1L),
      grp_assgn = val_match(grp_assgn, "N" ~ 0L, "Y" ~ 1L)) |>
    slt(
      -adr_ln_1,
      -adr_ln_2,
      -sec_spec_1,
      -sec_spec_2,
      -sec_spec_3,
      -sec_spec_4,
      -adrs_id,
      -ln_2_sprs
    ) |>
    rnm(pro_names("clinicians")) |>
    as_tbl()
}

#' Utilization Class
#' @param npi `<chr>` 10-digit Individual NPI
#' @param pac `<chr>` 10-digit PECOS Associate Control (PAC) ID
#' @param first_name,middle_name,last_name,suffix `<chr>` Individual provider's name
#' @param procedure `<chr>` Procedure category
#' @param count `<int>` Number of procedures performed
#' @param percentile `<int>` Percentile of procedures performed
#' @returns An S7 `<proUtilization>` object.
#' @examples
#' proUtilization(last_name = "CURRY")
#' proUtilization(last_name = "CURRY", middle_name = "")
#' @autoglobal
#' @export
proUtilization <- new_class(
  name       = "proUtilization",
  package    = NULL,
  properties = list(
    args = new_property(
      class_list,
      getter = function(self)
        format_query_pro(self@args)),
    request = new_property(
      class_list,
      getter = function(self)
        base_request(self@request) |>
        req_url_query(!!!self@args)),
    count = new_property(
      class_integer,
      getter = function(self)
        req_url_query(
          self@request,
          count = "true",
          results = "false") |>
        perform_simple() |>
        _[["count"]]),
    response = new_property(
      new_union(NULL, class_list),
      getter = function(self) {

        if (self@count == 0L) return(NULL)

        perform_simple(self@request) |>
        _[["results"]]

      })),
  constructor = function(npi         = NULL,
                         pac         = NULL,
                         last_name   = NULL,
                         first_name  = NULL,
                         middle_name = NULL,
                         suffix      = NULL,
                         procedure   = NULL,
                         count       = NULL,
                         percentile  = NULL) {

    new_object(
      S7_object(),
      args = list2(
        "npi"                  = npi,
        "ind_pac_id"           = pac,
        "provider_last_name"   = last_name,
        "provider_first_name"  = first_name,
        "provider_middle_name" = middle_name,
        "suff"                 = suffix,
        "procedure_category"   = procedure,
        "count"                = count,
        "percentile"           = percentile),
      request = pro_endpoint("PDC_utilization")
      )
  }
)

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

  req <- pro_endpoint("PDC_utilization") |> base_request() |> req_url_query(!!!args)

  n <- req_url_query(req, count = "true", results = "false") |> perform_simple() |> _[["count"]]

  if (!n) {
    return(NULL)
  }

  if (n) {
    req_url_query(req, count = "false", results = "true") |>
      perform_simple() |>
      _[["results"]] |>
      map_na_if() |>
      rnm(pro_names("utilization")) |>
      as_tbl()
  }
}

#' @noRd
#' @autoglobal
pro_names <- function(x, call = caller_env()) {
  switch(
    x,
    affiliations = c(
      ind_pac_id                                 = "pac",
      provider_last_name                         = "last_name",
      provider_first_name                        = "first_name",
      provider_middle_name                       = "middle_name",
      facility_affiliations_certification_number = "ccn_facility",
      facility_type_certification_number         = "ccn_primary"
    ),
    clinicians = c(
      ind_pac_id                                 = "pac",
      ind_enrl_id                                = "enid",
      provider_last_name                         = "last_name",
      provider_first_name                        = "first_name",
      provider_middle_name                       = "middle_name",
      suff                                       = "suffix_name",
      cred                                       = "credential",
      gndr                                       = "gender",
      grd_yr                                     = "grad_year",
      med_sch                                    = "med_school",
      pri_spec                                   = "spec_prim",
      sec_spec_all                               = "spec_sec",
      telehlth                                   = "telehealth",
      org_pac_id                                 = "org_pac",
      num_org_mem                                = "org_members",
      citytown                                   = "city",
      state                                      = "state",
      zip_code                                   = "zip",
      telephone_number                           = "phone"
    ),
    utilization = c(
      ind_pac_id                                 = "pac",
      provider_last_name                         = "last_name",
      provider_first_name                        = "first_name",
      provider_middle_name                       = "middle_name",
      profile_display_indicator                  = "display"
    ),
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
}
