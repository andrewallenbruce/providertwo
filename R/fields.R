# x <- endpoint("pdc_affiliations")
# x@dimensions@fields |>
# list_modify(prov_fields("affiliations"))
#' @autoglobal
#' @noRd
prov_fields <- function(x) {
  switch(
    x,
    affiliations = list(
      npi                  = "npi",
      ind_pac_id           = "pac_ind",
      provider_last_name   = "last_name",
      provider_first_name  = "first_name",
      provider_middle_name = "middle_name",
      suff                 = "suffix_name",
      facility_type        = factor(
        c(
          "Hospital",
          "Long-term care hospital",
          "Nursing home",
          "Inpatient rehabilitation facility",
          "Home health agency",
          "Hospice",
          "Skilled nursing facility",
          "Dialysis facility"
        )
      ),
      facility_affiliations_certification_number = "ccn",
      facility_type_certification_number = "ccn"
    ),
    clinicians = list(
      npi = list(type = "npi", form = "integer(10)"),
      ind_pac_id = list(type = "pac_ind", form = "character(10)"),
      ind_enrl_id = list(type = "enid_ind", form = "character(15)"),
      provider_last_name = list(type = "last_name", form = "character(0)"),
      provider_first_name = list(type = "first_name", form = "character(0)"),
      provider_middle_name = list(type = "middle_name", form = "character(0)"),
      suff = list(type = "suffix_name", form = "character(0)"),
      gndr = list(type = "gender", form = factor(c("F", "M"))),
      cred = list(type = "credentials", form = "character(0)"),
      med_sch = character(0),
      grd_yr = list(type = "year", form = "integer(4)"),
      pri_spec = list(type = "specialty", form = "character(0)"),
      sec_spec_1 = list(type = "specialty", form = "character(0)"),
      sec_spec_2 = list(type = "specialty", form = "character(0)"),
      sec_spec_3 = list(type = "specialty", form = "character(0)"),
      sec_spec_4 = list(type = "specialty", form = "character(0)"),
      sec_spec_all = list(type = "specialty", form = "character(0)"),
      telehlth = list(type = "enum", form = factor(c("Y", "M", "N"))),
      facility_name = list(type = "facility_name", form = "character(0)"),
      org_pac_id = list(type = "pac_org", form = "character(10)"),
      num_org_mem = list(type = "count", form = "integer(0)"),
      adr_ln_1 = list(type = "address_1", form = "character(0)"),
      adr_ln_2 = list(type = "address_2", form = "character(0)"),
      ln_2_sprs = logical(0),
      citytown = list(type = "city", form = "character(0)"),
      state = list(type = "state", form = "character(2)"),
      zip_code = list(type = "zip", form = "character(9)"),
      telephone_number = list(type = "phone", form = "integer(10)"),
      ind_assgn = list(type = "enum", form = factor(c("Y", "M", "N"))),
      grp_assgn = list(type = "enum", form = factor(c("Y", "M", "N"))),
      adrs_id = character(0)
    ),
    utilization = list(
      npi = "npi",
      ind_pac_id = "pac_ind",
      provider_last_name = "last_name",
      provider_first_name = "first_name",
      provider_middle_name = "middle_name",
      suff = "suffix_name",
      procedure_category = character(0),
      count = integer(0),
      percentile = integer(0),
      profile_display_indicator = character(0)
    )
  )
}
