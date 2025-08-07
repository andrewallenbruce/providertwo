#' @autoglobal
#' @noRd
field_keys <- S7::new_generic("field_keys", "obj", function(obj) {
  S7::S7_dispatch()
})

S7::method(field_keys, class_group) <- function(obj) {
  S7::prop(obj, "members") |> purrr::map(field_keys)
}

S7::method(field_keys, class_catalog) <- function(obj) {
  S7::prop(obj, "access") |> field_keys()
}

S7::method(field_keys, class_endpoint) <- function(obj) {
  S7::prop(obj, "fields") |> field_keys()
}

S7::method(field_keys, class_fields) <- function(obj) {
  S7::prop(obj, "keys")
}

#' @autoglobal
#' @noRd
enum_choices <- function(x) {
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
      "pac",
      "last_name",
      "first_name",
      "middle_name",
      "suffix",
      "facility_type",
      "unit_ccn",
      "primary_ccn"
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
      enum_choices(facility_types),
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
  # if (count < 11) "1-10"

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
      "pac",
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
      enum_choices(c("Y", "N")),
      rep(NA_character_, 2),
      enum_choices(profile)
    )
  )
}
