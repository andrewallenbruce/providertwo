#' @noRd
alias_main <- function(x) {
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
    "skilled_nursing",       "Skilled Nursing Facility Enrollments",
    default = NA_character_,
    nThread = 4L
  )
}

#' @noRd
alias_main_group <- function(x) {
  nswitch(
    x,
    "hospitals", "^Hospital",
    "rhc",       "Rural Health Clinic",
    "fqhc",      "Federally Qualified Health Center",
    "pending",   "Pending Initial Logging and Tracking",
    default = NA_character_,
    nThread = 4L
  )
}

#' @noRd
alias_main_temp <- function(x) {
  nswitch(
    x,
    "quality_payment",  "Quality Payment Program Experience",
    default = NA_character_,
    nThread = 4L
  )
}

#' @noRd
alias_main_temp_group <- function(x) {
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
alias_pro <- function(x) {
  nswitch(
    x,
    "affiliations",  "^Facility Affiliation Data$",
    "clinicians",    "^National Downloadable File$",
    "utilization",   "^Utilization Data$",
    default = NA_character_,
    nThread = 4L
  )
}

#' @noRd
alias_pro_group <- function(x) {
  nswitch(
    x,
    "mips", "^PY 2022",
    default = NA_character_,
    nThread = 4L
  )
}

#' @noRd
alias_open <- function(x) {
  nswitch(
    x,
    "prof_cov",        "^Covered Recipient Profile Supplement",
    "prof_phys",       "^Physician \\(Distinct\\) Profile Information",
    "prof_info",       "^Profile Information",
    "prof_map",        "^Provider Profile ID Mapping Table",
    "prof_entity",     "^Reporting Entity Profile Information",
    "prof_teach",      "^Teaching Hospital Profile Information",
    "dashboard",       "^Summary Dashboard",
    "pay_state_total", "^State Level Payment Total and Averages for all Years$",
    "pay_state_group", "^State Payment Totals and Averages Grouped by Nature of Payment for all Years$",
    "pay_nat_group",   "^National Level Payment Total and Averages by Provider Specialty for all Years$",
    "pay_nat_total",   "^National Level Payment Total and Averages for all Years$",
    default = NA_character_,
    nThread = 4L
  )
}

#' @noRd
alias_open_temp <- function(x) {
  nswitch(
    x,
    "general",                 "^General Payment Data$",
    "ownership",               "^Ownership Payment Data$",
    "research",                "^Research Payment Data$",
    "recipient_nature",        "^Payments Grouped by Covered Recipient and Nature of Payments$",
    "recipient_entity",        "^Payments Grouped by Covered Recipient and Reporting Entities$",
    "entity_nature",           "^Payments Grouped by Reporting Entities and Nature of Payments$",
    "entity_recipient_nature", "^Payments Grouped by Reporting Entities, Covered Recipient, and Nature of Payments$",
    "state_nature",            "^State Payment Totals Grouped by Nature of Payment for all Years$",
    default = NA_character_,
    nThread = 4L
  )
}
