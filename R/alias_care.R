#' @autoglobal
#' @noRd
select_care <- function(x, call = caller_env()) {

  x <- switch(
    x,
    contact = "Public Reporting of Missing Digital Contact Information",
    crosswalk = "Medicare Provider and Supplier Taxonomy Crosswalk",
    CARE_dialysis = "Medicare Dialysis Facilities",
    enrollees = "Public Provider Enrollment",
    facilities = "Provider of Services File - Hospital & Non-Hospital Facilities",
    IQIES = "Provider of Services File - Internet Quality Improvement and Evaluation System - Home Health Agency, Ambulatory Surgical Center, and Hospice Providers",
    laboratories = "Provider of Services File - Clinical Laboratories",
    long_term = "Long-Term Care Facility Characteristics",
    opt_out = "Opt Out Affidavits",
    order_refer = "Order and Referring",
    RBCS = "Restructured BETOS Classification System",
    transparency = "Hospital Price Transparency Enforcement Activities and Outcomes",
    HHA_owners = "^Home Health Agency All Owners$",
    HHA_cost_report = "^Home Health Agency Cost Report$",
    HHA_enrollments = "^Home Health Agency Enrollments$",
    hospice_owners = "^Hospice All Owners$",
    hospice_enrollments = "^Hospice Enrollments$",
    hospice_acute = "Medicare Post-Acute Care and Hospice - by Geography & Provider",
    hospital_owners = "^Hospital All Owners$",
    hospital_chow = "^Hospital Change of Ownership$",
    hospital_chow_owner = "^Hospital Change of Ownership - Owner Information$",
    hospital_enrollments = "^Hospital Enrollments$",
    RHC_owners = "^Rural Health Clinic All Owners$",
    RHC_enrollments = "^Rural Health Clinic Enrollments$",
    FQHC_owners = "^Federally Qualified Health Center All Owners$",
    FQHC_enrollments = "^Federally Qualified Health Center Enrollments$",
    PILAT_non_physicians = "^Pending Initial Logging and Tracking Non Physicians$",
    PILAT_physicians = "^Pending Initial Logging and Tracking Physicians$",
    REVAL_group = "^Revalidation Clinic Group Practice Reassignment$",
    REVAL_due_date = "^Revalidation Due Date List$",
    REVAL_reassignment = "^Revalidation Reassignment List$",
    SNF_owners = "^Skilled Nursing Facility All Owners$",
    SNF_chow = "^Skilled Nursing Facility Change of Ownership$",
    SNF_chow_owner = "^Skilled Nursing Facility Change of Ownership - Owner Information$",
    SNF_cost_report = "^Skilled Nursing Facility Cost Report$",
    SNF_enrollments = "^Skilled Nursing Facility Enrollments$",


    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )

  if (!exists("catalog")) .catalog <- catalogs()

  res <- select_alias(.catalog$care$main, x)

  if (empty(res))     cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  if (nrow(res) > 1L) cli_abort(c("x" = "> 1 match found for {.val {x}}."), call = call)

  c(res)

}

# program_stats   = "^CMS Program Statistics",
# care_group("program_stats")
#' @autoglobal
#' @noRd
select_care_group <- function(x, call = caller_env()) {
  switch(
    x,
    HHA = list(
      group = "Home Health Agencies",
      alias = c(
        "HHA_owners",
        "HHA_cost_report",
        "HHA_enrollments"
      )
    ),
    hospice = list(
      group = "Hospices",
      alias = c(
        "hospice_owners",
        "hospice_enrollments",
        "hospice_acute"
      )
    ),
    hospital = list(
      group = "Hospitals",
      alias = c(
        "hospital_owners",
        "hospital_chow",
        "hospital_chow_owner",
        "hospital_enrollments"
      )
    ),
    RHC = list(
      group = "Rural Health Clinics",
      alias = c(
        "RHC_owners",
        "RHC_enrollments"
      )
    ),
    FQHC = list(
      group = "Federally Qualified Health Centers",
      alias = c(
        "FQHC_owners",
        "FQHC_enrollments"
      )
    ),
    pending = list(
      group = "Pending Initial Logging and Tracking",
      alias = c(
        "PILAT_non_physicians",
        "PILAT_physicians"
      )
    ),
    reassignment = list(
      group = "Revalidation Reassignment Lists",
      alias = c(
        "REVAL_group",
        "REVAL_due_date",
        "REVAL_reassignment"
      )
    ),
    SNF = list(
      group = "Skilled Nursing Facilities",
      alias = c(
        "SNF_owners",
        "SNF_chow",
        "SNF_chow_owner",
        "SNF_cost_report",
        "SNF_enrollments"
      )
    ),
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
}

#' @autoglobal
#' @noRd
select_care_temp <- function(x, call = caller_env()) {

  x <- switch(
    x,
    quality_payment            = "^Quality Payment Program Experience$",
    IN_geography_and_service   = "^Medicare Inpatient Hospitals - by Geography and Service$",
    IN_provider                = "^Medicare Inpatient Hospitals - by Provider$",
    IN_provider_and_service    = "^Medicare Inpatient Hospitals - by Provider and Service$",
    OUT_geography_and_service  = "^Medicare Outpatient Hospitals - by Geography and Service$",
    OUT_provider_and_service   = "^Medicare Outpatient Hospitals - by Provider and Service$",
    PRX_geography_and_drug     = "^Medicare Part D Prescribers - by Geography and Drug$",
    PRX_provider               = "^Medicare Part D Prescribers - by Provider$",
    PRX_provider_and_drug      = "^Medicare Part D Prescribers - by Provider and Drug$",
    DME_geography_and_service  = "^Medicare Durable Medical Equipment, Devices & Supplies - by Geography and Service$",
    DME_provider               = "^Medicare Durable Medical Equipment, Devices & Supplies - by Referring Provider$",
    DME_provider_and_service   = "^Medicare Durable Medical Equipment, Devices & Supplies - by Referring Provider and Service$",
    DME_supplier               = "^Medicare Durable Medical Equipment, Devices & Supplies - by Supplier$",
    DME_supplier_and_service   = "^Medicare Durable Medical Equipment, Devices & Supplies - by Supplier and Service$",
    STAFF_non_nurse            = "^Payroll Based Journal Daily Non-Nurse Staffing$",
    STAFF_nurse                = "^Payroll Based Journal Daily Nurse Staffing$",
    STAFF_employee             = "^Payroll Based Journal Employee Detail Nursing Home Staffing$",
    UTIL_geography_and_service = "^Medicare Physician & Other Practitioners - by Geography and Service$",
    UTIL_provider              = "^Medicare Physician & Other Practitioners - by Provider$",
    UTIL_provider_and_service  = "^Medicare Physician & Other Practitioners - by Provider and Service$",
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call))

  if (!exists("catalog")) .catalog <- catalogs()

  res <- select_alias(.catalog$care$temp, x)

  if (empty(res)) cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)

  list_tidy(
    !!!c(slt(res, -data)),
    endpoints   = get_elem(res, "data") |> _[[1]],
    identifier  = endpoints$identifier[1]
  )

}

#' @autoglobal
#' @noRd
select_care_troup <- function(x, call = caller_env()) {
  switch(
    x,
    inpatient   = list(
      group = "Medicare Inpatient Hospitals",
      alias = c(
        "IN_geography_and_service",
        "IN_provider",
        "IN_provider_and_service"
      )
    ),
    outpatient  = list(
      group = "Medicare Outpatient Hospitals",
      alias = c(
        "OUT_geography_and_service",
        "OUT_provider_and_service"
      )
    ),
    prescribers = list(
      group = "Medicare Part D Prescribers",
      alias = c(
        "PRX_geography_and_drug",
        "PRX_provider",
        "PRX_provider_and_drug"
      )
    ),
    suppliers   = list(
      group = "Medicare Durable Medical Equipment, Devices & Supplies",
      alias = c(
        "DME_geography_and_service",
        "DME_provider",
        "DME_provider_and_service",
        "DME_supplier",
        "DME_supplier_and_service"
      )
    ),
    staffing    = list(
      group = "Nursing Home Payroll-Based Journal Staffing",
      alias = c(
        "STAFF_non_nurse",
        "STAFF_nurse",
        "STAFF_employee"
      )
    ),
    utilization = list(
      group = "Medicare Physician & Other Practitioners",
      alias = c(
        "UTIL_geography_and_service",
        "UTIL_provider",
        "UTIL_provider_and_service"
      )
    ),
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
}
