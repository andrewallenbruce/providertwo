#' @title Endpoint Aliases
#' @name alias
#' @param x `<chr>` alias for endpoint title
#' @autoglobal
#' @noRd
NULL

#' @noRd
alias_main_current <- function(x) {
  nswitch(
    x,
    "enrollees",             "Public Provider Enrollment",
    "opt_out",               "Opt Out Affidavits",
    "order_refer",           "Order and Referring",
    "reassignments",         "Revalidation Reassignment List",
    "hospitals",             "Hospital Enrollments",
    "laboratories",          "Provider of Services File - Clinical Laboratories",
    "facilities",            "Provider of Services File - Hospital & Non-Hospital Facilities",
    "crosswalk",             "Medicare Provider and Supplier Taxonomy Crosswalk",
    "rbcs",                  "Restructured BETOS Classification System",
    "home_health",           "Home Health Agency Enrollments",
    "hospice",               "Hospice Enrollments",
    "dialysis",              "Medicare Dialysis Facilities",
    "snf",                   "Skilled Nursing Facility Enrollments",
    default = NA_character_,
    nThread = 4L
  )
}

#' @noRd
alias_main_current_group <- function(x) {
  nswitch(
    x,
    "hospitals", "Hospital",
    "rhc",       "Rural Health Clinic",
    "fqhc",      "Federally Qualified Health Center",
    "pending",   "Pending Initial Logging and Tracking",
    default = NA_character_,
    nThread = 4L
  )
}

#' @noRd
alias_main_temporal <- function(x) {
  nswitch(
    x,
    "quality_payment",  "Quality Payment Program Experience",
    default = NA_character_,
    nThread = 4L
  )
}

#' @noRd
alias_main_temporal_group <- function(x) {
  nswitch(
    x,
    "prescribers", "Medicare Part D Prescribers",
    "utilization", "Medicare Physician & Other Practitioners",
    "outpatient",  "Medicare Outpatient Hospitals",
    "inpatient",   "Medicare Inpatient Hospitals",
    "suppliers",   "Medicare Durable Medical Equipment, Devices & Supplies",
    default = NA_character_,
    nThread = 4L
  )
}

#' @noRd
alias_provider <- function(x) {
  nswitch(
    x,
    "affiliations",  "Facility Affiliation Data",
    "clinicians",    "National Downloadable File",
    "utilization",   "Utilization Data",
    "group_mips",    "PY 2022 Group Public Reporting: MIPS Measures and Attestations",
    "group_patient", "PY 2022 Group Public Reporting: Patient Experience",
    "clin_mips",     "PY 2022 Clinician Public Reporting: MIPS Measures and Attestations",
    "clin_overall",  "PY 2022 Clinician Public Reporting: Overall MIPS Performance",
    "vgroup_mips",   "PY 2022 Virtual Group Public Reporting: MIPS Measures and Attestations",
    default = NA_character_,
    nThread = 4L
  )
}

#' @noRd
alias_open_current <- function(x) {
  nswitch(
    x,
    "profile_covr",   "Covered Recipient Profile Supplement",
    "profile_phys",   "Physician (Distinct) Profile Information",
    "profile_info",   "Profile Information",
    "profile_map",    "Provider Profile ID Mapping Table",
    "profile_entity", "Reporting Entity Profile Information",
    "profile_teach",  "Teaching Hospital Profile Information",
    default = NA_character_,
    nThread = 4L
  )
}

#' @noRd
alias_open_temporal <- function(x) {
  nswitch(
    x,
    "affiliations", "Facility Affiliation Data",
    "clinicians",   "National Downloadable File",
    default = NA_character_,
    nThread = 4L
  )
}
