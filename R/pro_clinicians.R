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

  log_info("Loading endpoint...")
  x <- pro_endpoint("pdc_clinicians")
  log_info("Querying {x@metadata$title}...")
  x <- base_request(x) |>
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
  log_info("Returned {nrow(x)} results")
  x
}
