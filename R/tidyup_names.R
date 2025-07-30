#' @noRd
#' @autoglobal
care_names <- function(x, call = caller_env()) {
  switch(
    x,
    enrollees = c(
      NPI                = "npi",
      PECOS_ASCT_CNTL_ID = "pac_ind",
      ENRLMT_ID          = "enid_ind",
      PROVIDER_TYPE_CD   = "spec_code",
      PROVIDER_TYPE_DESC = "spec_desc",
      STATE_CD           = "state",
      FIRST_NAME         = "first_name",
      MDL_NAME           = "mdle_name",
      LAST_NAME          = "last_name",
      ORG_NAME           = "org_name"
    ),
    cli::cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
}

#' @noRd
#' @autoglobal
prov_names <- function(x, call = caller_env()) {
  switch(
    x,
    affiliations = c(
      ind_pac_id                                 = "pac_ind",
      provider_last_name                         = "last_name",
      provider_first_name                        = "first_name",
      provider_middle_name                       = "mdl_name",
      facility_affiliations_certification_number = "ccn_facility",
      facility_type_certification_number         = "ccn_primary"
    ),
    clinicians = c(
      ind_pac_id                                 = "pac_ind",
      ind_enrl_id                                = "enid_ind",
      provider_last_name                         = "last_name",
      provider_first_name                        = "first_name",
      provider_middle_name                       = "mdl_name",
      suff                                       = "suff_name",
      cred                                       = "credential",
      gndr                                       = "gender",
      grd_yr                                     = "grad_year",
      med_sch                                    = "med_school",
      pri_spec                                   = "spec_prim",
      sec_spec_all                               = "spec_sec",
      telehlth                                   = "tele",
      org_pac_id                                 = "pac_org",
      num_org_mem                                = "org_members",
      citytown                                   = "city",
      state                                      = "state",
      zip_code                                   = "zip",
      telephone_number                           = "phone"
    ),
    utilization = c(
      ind_pac_id                                 = "pac_ind",
      provider_last_name                         = "last_name",
      provider_first_name                        = "first_name",
      provider_middle_name                       = "mdl_name",
      profile_display_indicator                  = "profile"
    ),
    cli::cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
}

#' @autoglobal
#' @noRd
qpp_name <- function(x, call = caller_env()) {
  switch(
    x,
    base = c(
      firstName                      = "first_name",
      middleName                     = "mdl_name",
      lastName                       = "last_name",
      nationalProviderIdentifierType = "entity",
      newlyEnrolled                  = "is_new",
      firstApprovedDate              = "date_appr",
      pecosEnrollmentDate            = "date_pac",
      yearsInMedicare                = "years_mcr",
      specialty_description          = "spec_desc",
      specialty_type                 = "spec_type",
      specialty_category             = "spec_cat",
      isMaqi                         = "is_maqi",
      qpStatus                       = "qp_status",
      amsMipsEligibleClinician       = "ams_mips_elig",
      organizations                  = "ORGS"
    ),
    organizations = c(
      ind_pac_id                     = "pac_ind",
      ind_enrl_id                    = "enid_ind",
      provider_last_name             = "last_name",
      provider_first_name            = "first_name",
      provider_middle_name           = "mdl_name",
      suff                           = "suff_name",
      cred                           = "credential",
      gndr                           = "gender",
      grd_yr                         = "grad_year",
      med_sch                        = "med_school",
      pri_spec                       = "spec_prim",
      sec_spec_all                   = "spec_sec",
      telehlth                       = "tele",
      org_pac_id                     = "pac_org",
      num_org_mem                    = "org_members",
      citytown                       = "city",
      state                          = "state",
      zip_code                       = "zip",
      telephone_number               = "phone"
    ),
    individualScenario = c(
      ind_pac_id                                 = "pac_ind",
      provider_last_name                         = "last_name",
      provider_first_name                        = "first_name",
      provider_middle_name                       = "mdl_name",
      profile_display_indicator                  = "profile"
    ),
    cli::cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
}
