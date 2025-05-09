#' @noRd
#' @autoglobal
care_names <- function(x, call = caller_env()) {
  switch(
    x,
    enrollees = c(
      NPI                = "npi",
      PECOS_ASCT_CNTL_ID = "pac",
      ENRLMT_ID          = "enid",
      PROVIDER_TYPE_CD   = "specialty_code",
      PROVIDER_TYPE_DESC = "specialty_description",
      STATE_CD           = "state",
      FIRST_NAME         = "first_name",
      MDL_NAME           = "middle_name",
      LAST_NAME          = "last_name",
      ORG_NAME           = "org_name"
    ),
    cli::cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
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
    cli::cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
}
