#' @autoglobal
#' @noRd
enumerate <- function(x) {
  glue::glue("c({toString(glue::single_quote(x))})")
}

#' @autoglobal
#' @noRd
aff_fields <- function() {
  facility_types <- c(
    "Hospital",
    "Long-term care hospital",
    "Nursing home",
    "Inpatient rehabilitation facility",
    "Home health agency",
    "Hospice",
    "Skilled nursing facility",
    "Dialysis facility"
  )

  fastplyr::new_tbl(
    alias = "pdc_affiliation",
    field = c(
      "npi",
      "ind_pac_id",
      "provider_last_name",
      "provider_first_name",
      "provider_middle_name",
      "suff",
      "facility_type",
      "facility_affiliations_certification_number",
      "facility_type_certification_number"
    ),
    param = c(
      "npi",
      "pac_ind",
      "last_name",
      "first_name",
      "middle_name",
      "suffix",
      "facility_type",
      "ccn_unit",
      "ccn_primary"
    ),
    constant = c(
      "npi",
      "pac",
      "name",
      "name",
      "name",
      "name",
      "enum",
      "ccn",
      "ccn"
    ),
    subtype = c(
      "individual",
      "individual",
      "individual",
      "individual",
      "individual",
      "individual",
      "individual",
      "numeric",
      "character"
    ),
    choices = c(
      rep(NA_character_, 6),
      enumerate(facility_types),
      rep(NA_character_, 2)
    ),
    definition = c(
      "Unique clinician ID assigned by NPPES",
      "Unique individual clinician ID assigned by PECOS",
      "Individual clinician last name",
      "Individual clinician first name",
      "Individual clinician middle name",
      "Individual clinician suffix",
      "Facilities can fall into the following type categories: Hospitals, Long-term Care Hospital, Nursing Home, Inpatient Rehabilitation Facility, Home Health Agency, Hospice and Dialysis Facility",
      "Medicare CCN of facility type or unit within hospital where an individual clinician provides service",
      "Medicare CCN of the primary hospital where individual clinician provides service, should the clinician provide services in a unit within the hospital"
    )
  )
}

#' @autoglobal
#' @noRd
util_fields <- function() {

  # TODO may not be all procedures
  procedure <- c(
    "Cataract surgery",
    "Colonoscopy",
    "Coronary angioplasty and stenting",
    "Coronary artery bypass graft (CABG)",
    "Hernia repair (minimally invasive)",
    "Hernia repair - groin (open)",
    "Hip replacement",
    "Knee replacement",
    "Laminectomy or laminotomy (partial removal of spine bones)",
    "Leg revascularization (restoring blood flow)",
    "Lower limb (leg) arthroscopy (minimally invasive joint repair)",
    "Mastectomy",
    "Melanoma (skin cancer) excision",
    "Pacemaker insertion or repair",
    "Prostate resection",
    "Spinal fusion",
    "Upper gastrointestinal (GI) endoscopy for acid reflux",
    "Upper limb (arm) arthroscopy (minimally invasive joint repair)",
    "Varicose vein removal"
  )

  # TODO count is ints
  # except for less than 11
  # which is "1-10"
  # ifelse(count < 11, "1-10", count)

  fastplyr::new_tbl(
    alias = "pdc_utilization",
    field = c(
      "npi",
      "ind_pac_id",
      "provider_last_name",
      "provider_first_name",
      "provider_middle_name",
      "suff",
      "procedure_category",
      "count",
      "percentile",
      "profile_display_indicator"
    ),
    param = c(
      "npi",
      "pac_ind",
      "last_name",
      "first_name",
      "middle_name",
      "suffix",
      "procedure",
      "count",
      "percentile",
      "profile_display"
    ),
    constant = c(
      "npi",
      "pac",
      "name",
      "name",
      "name",
      "name",
      "enum",
      NA_character_,
      NA_character_,
      "enum"
    ),
    subtype = c(
      "individual",
      "individual",
      "individual",
      "individual",
      "individual",
      "individual",
      "character",
      "integer",
      "integer",
      "character"
    ),
    choices = c(
      rep(NA_character_, 6),
      enumerate(procedure),
      glue::glue("ifelse(count < 11, '1-10', count)"),
      NA_character_,
      enumerate(c("Y", "N"))
    )
  )
}

#' @autoglobal
#' @noRd
clin_fields <- function() {

  ymn    <- enumerate(c("Y", "M", "N"))
  gender <- enumerate(c("F", "M"))

  fastplyr::new_tbl(
    alias = "pdc_clinician",
    field = c(
      "npi",
      "ind_pac_id",
      "ind_enrl_id",
      "provider_last_name",
      "provider_first_name",
      "provider_middle_name",
      "suff",
      "gndr",
      "cred",
      "med_sch",
      "grd_yr",
      "pri_spec",
      "sec_spec_1",
      "sec_spec_2",
      "sec_spec_3",
      "sec_spec_4",
      "sec_spec_all",
      "telehlth",
      "facility_name",
      "org_pac_id",
      "num_org_mem",
      "adr_ln_1",
      "adr_ln_2",
      "ln_2_sprs",
      "citytown",
      "state",
      "zip_code",
      "telephone_number",
      "ind_assgn",
      "grp_assgn",
      "adrs_id"
    ),
    param = c(
      "npi",
      "pac_ind",
      "enid_ind",
      "last_name",
      "first_name",
      "middle_name",
      "suffix",
      "gender",
      "credentials",
      "school_name",
      "grad_year",
      "specialty_primary",
      "sec_spec_1",
      "sec_spec_2",
      "sec_spec_3",
      "sec_spec_4",
      "specialty_other",
      "telehealth",
      "facility_name",
      "pac_org",
      "count_org",
      "address_1",
      "address_2",
      "add_press",
      "city",
      "state",
      "zip",
      "phone",
      "ind_assign",
      "group_assign",
      "id_add"
    ),
    constant = c(
      "npi",
      "pac",
      "enid",
      "name", # last
      "name", # first
      "name", # middle
      "name", # suffix
      "enum", # gender
      NA_character_, # credentials
      "name", # med school
      "year", # grad year
      "enum", # specialty primary
      rep(NA_character_, 4), # sec spec 1
      "enum", # specialty secondary
      "enum", # telehealth
      "name", # facility name
      "pac", # org pac
      NA_character_, # member count
      "address", # address 1
      "address", # address 2
      "enum", # address 2 suppressed
      "city",
      "state",
      "zip",
      "phone",
      "enum", # individual accepts assignment
      "enum", # group accepts assignment
      NA_character_ # address id
    ),
    subtype = c(
      "individual",
      "individual",
      "individual",
      "individual",
      "individual",
      "individual",
      "individual",
      "individual",
      "individual",
      "individual",
      "individual",
      "individual",
      NA_character_, # sec spec 1
      NA_character_,
      NA_character_,
      NA_character_,
      "individual",
      "organization",
      "organization",
      "organization",
      "organization",
      "organization",
      "organization",
      "organization",
      "organization",
      "organization",
      "organization",
      "organization",
      "individual",
      "organization",
      "organization"
    ),
    choices = c(
      rep(NA_character_, 7),
      gender,
      rep(NA_character_, 3),
      ymn, # TODO specialty primary
      rep(NA_character_, 4),
      ymn, # TODO specialty secondary
      ymn,
      rep(NA_character_, 5),
      ymn,
      rep(NA_character_, 4),
      ymn,
      ymn,
      NA_character_
    )
  )
}

#' @autoglobal
#' @noRd
pdc_fields <- function() {
  collapse::rowbind(fill = TRUE, aff_fields(), util_fields(), clin_fields())
}
