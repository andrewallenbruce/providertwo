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
    "enrollees",             "Public Provider Enrollment",  # "Medicare Fee-For-Service  Public Provider Enrollment",
    "opt_out",               "Opt Out Affidavits",
    "order_refer",           "Order and Referring",
    "reassignments",         "Revalidation Reassignment List",
    "hospitals",             "Hospital Enrollments",
    "laboratories",          "Provider of Services File - Clinical Laboratories",
    "crosswalk",             "Medicare Provider and Supplier Taxonomy Crosswalk",
    "rbcs",                  "Restructured BETOS Classification System",
    "pending_nonphysicians", "Pending Initial Logging and Tracking Non Physicians",
    "pending_physicians",    "Pending Initial Logging and Tracking Physicians",
    default = NA_character_,
    nThread = 4L
  )
}

#' @noRd
alias_main_temporal <- function(x) {
  nswitch(
    x,
    "prescribers_drug",      "Medicare Part D Prescribers - by Provider and Drug",
    "prescribers_provider",  "Medicare Part D Prescribers - by Provider",
    "prescribers_geography", "Medicare Part D Prescribers - by Geography and Drug",
    "utilization_service",   "Medicare Physician & Other Practitioners - by Provider and Service",
    "utilization_provider",  "Medicare Physician & Other Practitioners - by Provider$|Medicare Physician & Other Practitioners - by Provider : ", # "8889d81e-2ee7-448f-8713-f071038289b5",
    "utilization_geography", "Medicare Physician & Other Practitioners - by Geography and Service",
    default = NA_character_,
    nThread = 4L
  )
}

#' @noRd
alias_provider <- function(x) {
  nswitch(
    x,
    "affiliations", "Facility Affiliation Data",
    "clinicians",   "National Downloadable File",
    "utilization", "Utilization Data",
    default = NA_character_,
    nThread = 4L
  )
}

#' @noRd
alias_open_current <- function(x) {
  nswitch(
    x,
    "affiliations", "Facility Affiliation Data",
    "clinicians",   "National Downloadable File",
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
