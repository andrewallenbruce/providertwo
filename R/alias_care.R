# care_main("contact")
# care_main("crosswalk")
# care_main("dialysis")
# care_main("enrollees")
# care_main("facilities")
# care_main("hospice_acute")
# care_main("IQIES")
# care_main("laboratories")
# care_main("order_refer")
# care_main("laboratories")
# care_main("long_term")
# care_main("opt_out")
# care_main("order_refer")
# care_main("rbcs")
# care_main("transparency")
#' @autoglobal
#' @noRd
care_main <- function(x, call = caller_env()) {

  x <- switch(
    x,
    contact         = "Public Reporting of Missing Digital Contact Information",
    crosswalk       = "Medicare Provider and Supplier Taxonomy Crosswalk",
    dialysis        = "Medicare Dialysis Facilities",
    enrollees       = "Public Provider Enrollment",
    facilities      = "Provider of Services File - Hospital & Non-Hospital Facilities",
    hospice_acute   = "Medicare Post-Acute Care and Hospice - by Geography & Provider",
    IQIES           = "Provider of Services File - Internet Quality Improvement and Evaluation System - Home Health Agency, Ambulatory Surgical Center, and Hospice Providers",
    laboratories    = "Provider of Services File - Clinical Laboratories",
    long_term       = "Long-Term Care Facility Characteristics",
    opt_out         = "Opt Out Affidavits",
    order_refer     = "Order and Referring",
    rbcs            = "Restructured BETOS Classification System",
    transparency    = "Hospital Price Transparency Enforcement Activities and Outcomes",

    home_health_agency_all_owners                       = "^Home Health Agency All Owners$",
    home_health_agency_cost_report                      = "^Home Health Agency Cost Report$",
    home_health_agency_enrollments                      = "^Home Health Agency Enrollments$",
    hospice_all_owners                                  = "^Hospice All Owners$",
    hospice_enrollments                                 = "^Hospice Enrollments$",
    hospital_all_owners                                 = "^Hospital All Owners$",
    hospital_change_of_ownership                        = "^Hospital Change of Ownership$",
    hospital_change_of_ownership_owner_information      = "^Hospital Change of Ownership - Owner Information$",
    hospital_enrollments                                = "^Hospital Enrollments$",
    rural_health_clinic_all_owners                      = "^Rural Health Clinic All Owners$",
    rural_health_clinic_enrollments                     = "^Rural Health Clinic Enrollments$",
    federally_qualified_health_center_all_owners        = "^Federally Qualified Health Center All Owners$",
    federally_qualified_health_center_enrollments       = "^Federally Qualified Health Center Enrollments$",
    pending_initial_logging_and_tracking_non_physicians = "^Pending Initial Logging and Tracking Non Physicians$",
    pending_initial_logging_and_tracking_physicians     = "^Pending Initial Logging and Tracking Physicians$",
    revalidation_clinic_group_practice_reassignment     = "^Revalidation Clinic Group Practice Reassignment$",
    revalidation_due_date_list                          = "^Revalidation Due Date List$",
    revalidation_reassignment_list                      = "^Revalidation Reassignment List$",
    skilled_nursing_facility_all_owners                 = "^Skilled Nursing Facility All Owners$",
    skilled_nursing_facility_change_of_ownership        = "^Skilled Nursing Facility Change of Ownership$",
    skilled_nursing_facility_change_of_ownership_owner_information = "^Skilled Nursing Facility Change of Ownership - Owner Information$",
    skilled_nursing_facility_cost_report                = "^Skilled Nursing Facility Cost Report$",
    skilled_nursing_facility_enrollments                = "^Skilled Nursing Facility Enrollments$",

    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call))

  if (!exists("catalog")) .catalog <- catalogs()

  select_alias(.catalog$care$main, x) |> c()

}

# care_group("home_health")
# care_group("hospice")
# care_group("hospital")
# care_group("rural_health")
# care_group("fqhc")
# care_group("pending")
# care_group("program_stats")
# care_group("reassignment")
# care_group("skilled_nursing")
#' @autoglobal
#' @noRd
care_group <- function(x, call = caller_env()) {

  # program_stats   = "^CMS Program Statistics",
  switch(
    x,
    home_health     = list(group = "Home Health Agency",
                           alias = c("home_health_agency_all_owners",
                                     "home_health_agency_cost_report",
                                     "home_health_agency_enrollments")),
    hospice         = list(group = "Hospice",
                           alias = c("hospice_all_owners",
                                     "hospice_enrollments")),
    hospital        = list(group = "Hospitals",
                           alias = c("hospital_all_owners",
                                     "hospital_change_of_ownership",
                                     "hospital_change_of_ownership_owner_information",
                                     "hospital_enrollments")),
    rural_health    = list(group = "Rural Health Clinic",
                           alias = c("rural_health_clinic_all_owners",
                                     "rural_health_clinic_enrollments")),
    fqhc            = list(group = "Federally Qualified Health Center",
                           alias = c("federally_qualified_health_center_all_owners",
                                     "federally_qualified_health_center_enrollments")),
    pending         = list(group = "Pending Initial Logging and Tracking",
                           alias = c("pending_initial_logging_and_tracking_non_physicians",
                                     "pending_initial_logging_and_tracking_physicians")),
    reassignment    = list(group = "Revalidation Reassignment",
                           alias = c("revalidation_clinic_group_practice_reassignment",
                                     "revalidation_due_date_list",
                                     "revalidation_reassignment_list")),
    skilled_nursing = list(group = "Skilled Nursing Facility",
                           alias = c("skilled_nursing_facility_all_owners",
                                     "skilled_nursing_facility_change_of_ownership",
                                     "skilled_nursing_facility_change_of_ownership_owner_information",
                                     "skilled_nursing_facility_cost_report",
                                     "skilled_nursing_facility_enrollments")),
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call))
}

# care_temp("quality_payment")
#' @autoglobal
#' @noRd
care_temp <- function(x, call = caller_env()) {

  x <- switch(
    x,
    quality_payment                    = "Quality Payment Program Experience",
    inpatient_geography_and_service    = "^Medicare Inpatient Hospitals - by Geography and Service$",
    inpatient_provider                 = "^Medicare Inpatient Hospitals - by Provider$",
    inpatient_provider_and_service     = "^Medicare Inpatient Hospitals - by Provider and Service$",
    outpatient_geography_and_service   = "^Medicare Outpatient Hospitals - by Geography and Service$",
    outpatient_provider_and_service    = "^Medicare Outpatient Hospitals - by Provider and Service$",
    prescribers_geography_and_drug     = "^Medicare Part D Prescribers - by Geography and Drug$",
    prescribers_provider               = "^Medicare Part D Prescribers - by Provider$",
    prescribers_provider_and_drug      = "^Medicare Part D Prescribers - by Provider and Drug$",
    suppliers_geography_and_service    = "^Medicare Durable Medical Equipment, Devices & Supplies - by Geography and Service$",
    suppliers_provider                 = "^Medicare Durable Medical Equipment, Devices & Supplies - by Referring Provider$",
    suppliers_provider_and_service     = "^Medicare Durable Medical Equipment, Devices & Supplies - by Referring Provider and Service$",
    suppliers_supplier                 = "^Medicare Durable Medical Equipment, Devices & Supplies - by Supplier$",
    suppliers_supplier_and_service     = "^Medicare Durable Medical Equipment, Devices & Supplies - by Supplier and Service$",
    staffing_non_nurse                 = "^Payroll Based Journal Daily Non-Nurse Staffing$",
    staffing_nurse                     = "^Payroll Based Journal Daily Nurse Staffing$",
    staffing_employee_detail           = "^Payroll Based Journal Employee Detail Nursing Home Staffing$",
    utilization_geography_and_service  = "^Medicare Physician & Other Practitioners - by Geography and Service$",
    utilization_provider               = "^Medicare Physician & Other Practitioners - by Provider$",
    utilization_provider_and_service   = "^Medicare Physician & Other Practitioners - by Provider and Service$",
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call))

  if (!exists("catalog")) .catalog <- catalogs()

  x <- select_alias(.catalog$care$temp, x)

  l <- slt(x, -data) |> c()

  list(
    title       = l$title,
    description = l$description,
    periodicity = l$periodicity,
    contact     = l$contact,
    dictionary  = l$dictionary,
    site        = l$site,
    identifier  = get_elem(x, "data")[[1]]$identifier[1],
    endpoints   = get_elem(x, "data")[[1]]
  )

}

# care_temp_group("inpatient")
# care_temp_group("outpatient")
# care_temp_group("utilization")
# care_temp_group("suppliers")
# care_temp_group("prescribers")
# care_temp_group("staffing")
#' @autoglobal
#' @noRd
care_temp_group <- function(x, call = caller_env()) {

  switch(
    x,
    inpatient   = list(group = "Medicare Inpatient Hospitals",
                       alias = c("inpatient_geography_and_service",
                                 "inpatient_provider",
                                 "inpatient_provider_and_service")),
    outpatient  = list(group = "Medicare Outpatient Hospitals",
                       alias = c("outpatient_geography_and_service",
                                 "outpatient_provider_and_service")),
    prescribers = list(group = "Medicare Part D Prescribers",
                       alias = c("prescribers_geography_and_drug",
                                 "prescribers_provider",
                                 "prescribers_provider_and_drug")),
    suppliers   = list(group = "Medicare Durable Medical Equipment, Devices & Supplies",
                       alias = c("suppliers_geography_and_service",
                                 "suppliers_provider",
                                 "suppliers_provider_and_service",
                                 "suppliers_supplier",
                                 "suppliers_supplier_and_service")),
    staffing    = list(group = "Nursing Home Payroll-Based Journal Staffing",
                       alias = c("staffing_non_nurse",
                                 "staffing_nurse",
                                 "staffing_employee_detail")),
    utilization = list(group = "Medicare Physician & Other Practitioners",
                       alias = c("utilization_geography_and_service",
                                 "utilization_provider",
                                 "utilization_provider_and_service")),
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call))
}
