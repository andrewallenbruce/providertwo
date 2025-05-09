#' @autoglobal
#' @noRd
str_look <- function(pattern, look) {
  switch(
    match.arg(look, c("ahead", "behind")),
    ahead  = glue("(?<={pattern}).*$"),
    behind = glue("^.*(?={pattern})")
  )
}

#' @autoglobal
#' @noRd
str_look_detect <- function(x, pattern, look) {
  str_look(pattern, look) |>
    grepl(x, perl = TRUE)
}

#' @autoglobal
#' @noRd
str_look_replace <- function(x, pattern, look, replacement) {
  str_look(pattern, look) |>
    gsub(replacement = replacement, x, perl = TRUE)
}

#' @autoglobal
#' @noRd
str_look_remove <- function(x, pattern, look) {
  str_look_replace(x, pattern, look, replacement = "")
}


#' @autoglobal
#' @noRd
make_join_col <- \(x, col) {
  map(x[[ensym(col)]], function(x) get_elem(as.list(x), "data")) |>
    flatten_column() |>
    na_if("")
}

# care_types("single")
# care_types("multi")
#' @autoglobal
#' @noRd
care_types <- function(x) {
  switch(
    x,
    single = list(
      "ACO REACH Aligned Beneficiaries",
      "ACO REACH Eligible Beneficiaries",
      "ACO REACH Financial and Quality Results",
      "ACO REACH Providers",
      "COVID-19 Nursing Home Data",
      "CPC Initiative - Participating Primary Care Practices",
      "Comprehensive Care for Joint Replacement Model: Metropolitan Statistical Areas",
      "Innovation Center Data and Reports",
      "Innovation Center Innovation Advisors",
      "Innovation Center Milestones and Updates",
      "Innovation Center Model Awardees",
      "Innovation Center Model Participants",
      "Innovation Center Model Summary Information",
      "Innovation Center Webinars and Forums",
      "Market Saturation & Utilization Core-Based Statistical Areas",
      "Market Saturation & Utilization State-County",
      "Medicaid Managed Care",
      "Medicaid Opioid Prescribing Rates - by Geography",
      "Medicaid Spending by Drug",
      "Medicare Advantage Geographic Variation - National & State",
      "Medicare Clinical Laboratory Fee Schedule Private Payer Rates and Volumes",
      "Medicare Demonstrations",
      "Medicare Diabetes Prevention Program",
      "Medicare Geographic Variation - by National, State & County",
      "Medicare Monthly Enrollment",
      "Medicare Part B Discarded Drug Units",
      "Medicare Part B Spending by Drug",
      "Medicare Part D Opioid Prescribing Rates - by Geography",
      "Medicare Part D Spending by Drug",
      "Medicare Post-Acute Care and Hospice - by Geography & Provider",
      "Medicare Telehealth Trends",
      "Restructured BETOS Classification System",
      "Strong Start Awardees"
    ),
    multi = list(
      "Provider of Services File - Clinical Laboratories",
      "Provider of Services File - Hospital & Non-Hospital Facilities",
      "Medicare Fee-for-Service Comprehensive Error Rate Testing",
      "Physician/Supplier Procedure Summary",
      "Accountable Care Organization Participants",
      "Accountable Care Organizations",
      "Hospital Provider Cost Report",
      "Skilled Nursing Facility Cost Report",
      "Medicare Part D Prescribers - by Geography and Drug",
      "Medicare Part D Prescribers - by Provider",
      "Medicare Part D Prescribers - by Provider and Drug",
      "Medicare Physician & Other Practitioners - by Geography and Service",
      "Medicare Physician & Other Practitioners - by Provider",
      "Medicare Physician & Other Practitioners - by Provider and Service",
      "Performance Year Financial and Quality Results",
      "Deficit Reduction Act Hospital-Acquired Condition Measures",
      "Medicare Inpatient Hospitals - by Geography and Service",
      "Medicare Inpatient Hospitals - by Provider",
      "Medicare Inpatient Hospitals - by Provider and Service",
      "Accountable Care Organization Skilled Nursing Facility Affiliates",
      "Hospital Service Area",
      "Medicare Durable Medical Equipment, Devices & Supplies - by Geography and Service",
      "Medicare Durable Medical Equipment, Devices & Supplies - by Referring Provider",
      "Medicare Durable Medical Equipment, Devices & Supplies - by Referring Provider and Service",
      "Medicare Durable Medical Equipment, Devices & Supplies - by Supplier",
      "Medicare Durable Medical Equipment, Devices & Supplies - by Supplier and Service",
      "County-level Aggregate Expenditure and Risk Score Data on Assignable Beneficiaries",
      "Medicare Outpatient Hospitals - by Geography and Service",
      "Medicare Outpatient Hospitals - by Provider and Service",
      "Number of Accountable Care Organization Assigned Beneficiaries by County",
      "Payroll Based Journal Daily Non-Nurse Staffing",
      "Payroll Based Journal Daily Nurse Staffing",
      "Minimum Data Set Frequency",
      "Quality Payment Program Experience",
      "Medicare Dialysis Facilities",
      "Payroll Based Journal Employee Detail Nursing Home Staffing",
      "Pioneer ACO Model",
      "REACH ACOs",
      "Hospital All Owners",
      "Hospital Enrollments",
      "Medicare Provider and Supplier Taxonomy Crosswalk",
      "Revalidation Clinic Group Practice Reassignment",
      "Revalidation Due Date List",
      "Revalidation Reassignment List",
      "Skilled Nursing Facility All Owners",
      "Skilled Nursing Facility Enrollments",
      "Value Modifier",
      "End-Stage Renal Disease Facility Aggregation Group Performance",
      "Federally Qualified Health Center All Owners",
      "Federally Qualified Health Center Enrollments",
      "Fiscal Intermediary Shared System Attending and Rendering",
      "Home Health Agency All Owners",
      "Home Health Agency Cost Report",
      "Home Health Agency Enrollments",
      "Home Infusion Therapy Providers",
      "Hospice All Owners",
      "Hospice Enrollments",
      "Hospital Change of Ownership",
      "Hospital Change of Ownership - Owner Information",
      "Managing Clinician Aggregation Group Performance",
      "Medicare Fee-For-Service Public Provider Enrollment",
      "Nursing Home Affiliated Entity Performance Measures",
      "Opioid Treatment Program Providers",
      "Opt Out Affidavits",
      "Order and Referring",
      "Pending Initial Logging and Tracking Non Physicians",
      "Pending Initial Logging and Tracking Physicians",
      "Provider of Services File - Internet Quality Improvement and Evaluation System - Home Health Agency, Ambulatory Surgical Center, and Hospice Providers",
      "Rural Health Clinic All Owners",
      "Rural Health Clinic Enrollments",
      "Skilled Nursing Facility Change of Ownership",
      "Skilled Nursing Facility Change of Ownership - Owner Information",
      "Advance Investment Payment Spend Plan",
      "Agency for Healthcare Research and Quality (AHRQ) Patient Safety Indicator 11 (PSI-11) Measure Rates",
      "Facility-Level Minimum Data Set Frequency",
      "Hospital Price Transparency Enforcement Activities and Outcomes",
      "Long-Term Care Facility Characteristics",
      "Medicare COVID-19 Hospitalization Trends",
      "Public Reporting of Missing Digital Contact Information"
    ),
    cli::cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  ) |> unlist(use.names = FALSE)
}
