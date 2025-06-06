#' @autoglobal
#' @noRd
care_dimensions <- function(x, call = caller_env()) {

  req <- identifier_(x) |>
    request() |>
    req_url_query(offset = 0L, size = 1L) |>
    req_error(is_error = ~ FALSE)

  x <- switch(
    x$api,
    `Medicare` = perform_simple(req)$meta |> get_elem(c("total_rows", "headers")),
    `Medicare [Temporal]` = list(
      headers = perform_simple(req) |> names(),
      total_rows = req_url_path_append(req, "stats") |>
        perform_simple() |>
        get_elem("total_rows")
    )
  )
  class_dimensions(
    limit  = 5000L,
    rows   = x$total_rows %||% 0L,
    fields = x$headers %||% new_list(length = 1L, default = x$meta$message))
}

#' @autoglobal
#' @noRd
select_care <- function(x, call = caller_env()) {
  x <- switch(
    x,
    ahqr_psi11              = "^Agency for Healthcare Research and Quality \\(AHRQ\\) Patient Safety Indicator 11 \\(PSI[-]11\\) Measure Rates$",
    aip_plan                = "^Advance Investment Payment Spend Plan$",
    cpc_primary             = "^CPC Initiative [-] Participating Primary Care Practices$",
    cpc_joint               = "^Comprehensive Care for Joint Replacement Model[:] Metropolitan Statistical Areas$",
    county_risk_spend       = "^County[-]level Aggregate Expenditure and Risk Score Data on Assignable Beneficiaries$",
    hac_measures            = "^Deficit Reduction Act Hospital[-]Acquired Condition Measures$",
    esrd_group_perf         = "^End-Stage Renal Disease Facility Aggregation Group Performance$",
    fiss_rendering          = "^Fiscal Intermediary Shared System Attending and Rendering$",
    home_infusion           = "^Home Infusion Therapy Providers$",
    ffs_cert                = "^Medicare Fee[-]for[-]Service Comprehensive Error Rate Testing$",
    diabetes_prevention     = "^Medicare Diabetes Prevention Program$",
    clinician_group         = "^Managing Clinician Aggregation Group Performance$",
    lab_fee_schedule        = "^Medicare Clinical Laboratory Fee Schedule Private Payer Rates and Volumes$",
    aco_reach_aligned       = "^ACO REACH Aligned Beneficiaries$",
    aco_reach_eligible      = "^ACO REACH Eligible Beneficiaries$",
    aco_reach_results       = "^ACO REACH Financial and Quality Results$",
    aco_reach_providers     = "^ACO REACH Providers$",
    aco_reach_orgs          = "^REACH ACOs$",
    aco_pioneer             = "^Pioneer ACO Model$",
    aco_participants        = "^Accountable Care Organization Participants$",
    aco_snf_affiliate       = "^Accountable Care Organization Skilled Nursing Facility Affiliates$",
    aco_organizations       = "^Accountable Care Organizations$",
    aco_bene_cnty           = "^Number of Accountable Care Organization Assigned Beneficiaries by County$",
    care_dialysis           = "^Medicare Dialysis Facilities$",
    care_enrollees          = "Public Provider Enrollment",
    care_facilities         = "^Provider of Services File [-] Hospital & Non[-]Hospital Facilities$",
    care_pos_iqies          = "^Provider of Services File [-] Internet Quality Improvement and Evaluation System [-] Home Health Agency, Ambulatory Surgical Center, and Hospice Providers$",
    care_pos_labs           = "^Provider of Services File [-] Clinical Laboratories$",
    care_ltcf               = "^Long[-]Term Care Facility Characteristics$",
    care_opt_out            = "^Opt Out Affidavits$",
    care_order_refer        = "^Order and Referring$",
    care_restruct_betos     = "^Restructured BETOS Classification System$",
    care_miss_contact       = "^Public Reporting of Missing Digital Contact Information$",
    care_taxonomy_crosswalk = "^Medicare Provider and Supplier Taxonomy Crosswalk$",
    care_caid_managed_care  = "^Medicaid Managed Care",
    care_caid_opioid_geo    = "^Medicaid Opioid Prescribing Rates [-] by Geography",
    care_caid_drug_spend    = "^Medicaid Spending by Drug",
    care_benes              = "^Medicare Monthly Enrollment$",
    cms_ma_enroll           = "^CMS Program Statistics [-] Medicare Advantage [&] Other Health Plan Enrollment$",
    cms_ma_outpatient       = "^CMS Program Statistics [-] Medicare Advantage [-] Outpatient Facility$",
    cms_ma_other            = "^CMS Program Statistics [-] Medicare Advantage [-] Physician, Non[-]Physician Practitioner [&] Supplier$",
    cms_ma_inpatient        = "^CMS Program Statistics [-] Medicare Advantage[-]Inpatient Hospital$",
    cms_ma_snf              = "^CMS Program Statistics [-] Medicare Advantage[-]Skilled Nursing Facility$",
    cms_deaths              = "^CMS Program Statistics [-] Medicare Deaths$",
    cms_hha                 = "^CMS Program Statistics [-] Medicare Home Health Agency$",
    cms_hospice             = "^CMS Program Statistics [-] Medicare Hospice$",
    cms_inpatient           = "^CMS Program Statistics [-] Medicare Inpatient Hospital$",
    cms_new_enroll          = "^CMS Program Statistics [-] Medicare Newly Enrolled$",
    cms_outpatient          = "^CMS Program Statistics [-] Medicare Outpatient Facility$",
    cms_tos                 = "^CMS Program Statistics [-] Medicare Part A [&] Part B [-] All Types of Service$",
    cms_partd               = "^CMS Program Statistics [-] Medicare Part D$",
    cms_partd_enroll        = "^CMS Program Statistics [-] Medicare Part D Enrollment$",
    cms_phys_npp_supp       = "^CMS Program Statistics [-] Medicare Physician, Non[-]Physician Practitioner [&] Supplier$",
    cms_premiums            = "^CMS Program Statistics [-] Medicare Premiums$",
    cms_providers           = "^CMS Program Statistics [-] Medicare Providers$",
    cms_snf                 = "^CMS Program Statistics [-] Medicare Skilled Nursing Facility$",
    cms_total_enroll        = "^CMS Program Statistics [-] Medicare Total Enrollment$",
    cms_dual_enroll         = "^CMS Program Statistics [-] Medicare[-]Medicaid Dual Enrollment$",
    cms_orig_enroll         = "^CMS Program Statistics [-] Original Medicare Enrollment$",
    nhome_mds_facility      = "^Facility-Level Minimum Data Set Frequency$",
    nhome_mds_total         = "^Minimum Data Set Frequency$",
    market_cbsa             = "^Market Saturation [&] Utilization Core[-]Based Statistical Areas$",
    market_state_cnty       = "^Market Saturation [&] Utilization State[-]County$",
    partb_drug_discard      = "^Medicare Part B Discarded Drug Units$",
    partb_drug_spend        = "^Medicare Part B Spending by Drug$",
    partd_drug_spend        = "^Medicare Part D Spending by Drug$",
    partd_opioid            = "^Medicare Part D Opioid Prescribing Rates [-] by Geography$",
    bene_survey_covid       = "^Medicare Current Beneficiary Survey [-] COVID-19 Supplement$",
    bene_survey_cost        = "^Medicare Current Beneficiary Survey [-] Cost Supplement$",
    bene_survey_file        = "^Medicare Current Beneficiary Survey [-] Survey File$",
    geovar_adv              = "^Medicare Advantage Geographic Variation [-] National [&] State$",
    geovar_hrr              = "^Medicare Geographic Variation [-] by Hospital Referral Region$",
    geovar_nsc              = "^Medicare Geographic Variation [-] by National, State [&] County$",
    pdp_month               = "^Monthly Prescription Drug Plan Formulary and Pharmacy Network Information$",
    pdp_quarter             = "^Quarterly Prescription Drug Plan Formulary, Pharmacy Network, and Pricing Information$",
    hha_owners              = "^Home Health Agency All Owners$",
    hha_costreport          = "^Home Health Agency Cost Report$",
    hha_enrollments         = "^Home Health Agency Enrollments$",
    hospice_owners          = "^Hospice All Owners$",
    hospice_enrollments     = "^Hospice Enrollments$",
    hospice_acute           = "^Medicare Post[-]Acute Care and Hospice [-] by Geography [&] Provider",
    hospital_transparency   = "^Hospital Price Transparency Enforcement Activities and Outcomes$",
    hospital_owners         = "^Hospital All Owners$",
    hospital_chow           = "^Hospital Change of Ownership$",
    hospital_chow_owner     = "^Hospital Change of Ownership - Owner Information$",
    hospital_enrollments    = "^Hospital Enrollments$",
    hospital_costreport     = "^Hospital Provider Cost Report",
    hospital_service_area   = "^Hospital Service Area",
    rhc_owners              = "^Rural Health Clinic All Owners$",
    rhc_enrollments         = "^Rural Health Clinic Enrollments$",
    fqhc_owners             = "^Federally Qualified Health Center All Owners$",
    fqhc_enrollments        = "^Federally Qualified Health Center Enrollments$",
    pilat_non_physician     = "^Pending Initial Logging and Tracking Non Physicians$",
    pilat_physician         = "^Pending Initial Logging and Tracking Physicians$",
    revalid_group           = "^Revalidation Clinic Group Practice Reassignment$",
    revalid_due             = "^Revalidation Due Date List$",
    revalid_list            = "^Revalidation Reassignment List$",
    snf_owners              = "^Skilled Nursing Facility All Owners$",
    snf_chow                = "^Skilled Nursing Facility Change of Ownership$",
    snf_chow_owner          = "^Skilled Nursing Facility Change of Ownership [-] Owner Information$",
    snf_cost_report         = "^Skilled Nursing Facility Cost Report$",
    snf_enrollments         = "^Skilled Nursing Facility Enrollments$",
    cli::cli_abort(c("x" = "{.emph alias} {.val {x}} is invalid."), call = call)
  )

  res <- select_alias(the$catalog$care$main, x)

  if (is_empty(res))  cli::cli_abort(c("x" = "{.val {x}} returned no matches."), call = call)
  if (nrow(res) > 1L) cli::cli_abort(c("x" = "{.val {x}} returned more than 1 match."), call = call)

  c(res)

}

#' @autoglobal
#' @noRd
select_care_temp <- function(x, call = caller_env()) {

  x <- switch(
    x,
    quality_payment      = "^Quality Payment Program Experience$",
    procedure_summary    = "^Physician[/]Supplier Procedure Summary$",
    dialysis_facility    = "^Medicare Dialysis Facilities$",
    aco_shared_savings   = "^Performance Year Financial and Quality Results$",
    opioid_treatment     = "^Opioid Treatment Program Providers$",
    hospital_costreport  = "^Hospital Provider Cost Report$",
    snf_costreport       = "^Skilled Nursing Facility Cost Report$",
    hha_costreport       = "^Home Health Agency Cost Report$",
    nhome_performance    = "^Nursing Home Affiliated Entity Performance Measures$",
    nhome_mds_frequency  = "^Minimum Data Set Frequency$",
    nhome_mds_facility   = "^Facility[-]Level Minimum Data Set Frequency$",
    inpatient_geography  = "^Medicare Inpatient Hospitals [-] by Geography and Service$",
    inpatient_provider   = "^Medicare Inpatient Hospitals [-] by Provider$",
    inpatient_service    = "^Medicare Inpatient Hospitals [-] by Provider and Service$",
    outpatient_geography = "^Medicare Outpatient Hospitals [-] by Geography and Service$",
    outpatient_service   = "^Medicare Outpatient Hospitals [-] by Provider and Service$",
    prx_geography        = "^Medicare Part D Prescribers [-] by Geography and Drug$",
    prx_provider         = "^Medicare Part D Prescribers [-] by Provider$",
    prx_drug             = "^Medicare Part D Prescribers [-] by Provider and Drug$",
    dme_geography        = "^Medicare Durable Medical Equipment, Devices [&] Supplies [-] by Geography and Service$",
    dme_provider         = "^Medicare Durable Medical Equipment, Devices [&] Supplies [-] by Referring Provider$",
    dme_service          = "^Medicare Durable Medical Equipment, Devices [&] Supplies [-] by Referring Provider and Service$",
    dme_supplier         = "^Medicare Durable Medical Equipment, Devices [&] Supplies [-] by Supplier$",
    dme_supplier_service = "^Medicare Durable Medical Equipment, Devices [&] Supplies [-] by Supplier and Service$",
    nhome_staff_nonurse  = "^Payroll Based Journal Daily Non[-]Nurse Staffing$",
    nhome_staff_nurse    = "^Payroll Based Journal Daily Nurse Staffing$",
    nhome_staff_employee = "^Payroll Based Journal Employee Detail Nursing Home Staffing$",
    util_geography       = "^Medicare Physician [&] Other Practitioners [-] by Geography and Service$",
    util_provider        = "^Medicare Physician [&] Other Practitioners [-] by Provider$",
    util_service         = "^Medicare Physician [&] Other Practitioners [-] by Provider and Service$",
    cli::cli_abort(c("x" = "{.emph alias} {.val {x}} is invalid."), call = call)
  )

  res <- select_alias(the$catalog$care$temp, x)

  if (is_empty(res)) cli::cli_abort(c("x" = "{.val {x}} returned no matches."), call = call)

  list_tidy(
    !!!c(slt(res, -endpoints)),
    endpoints   = pluck(get_elem(res, "endpoints"), 1),
    identifier  = endpoints$identifier[1]
  )

}
