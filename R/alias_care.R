#' @autoglobal
#' @noRd
select_care <- function(x, call = caller_env()) {

  x <- switch(
    x,
    beneficiaries         = "^Medicare Monthly Enrollment$",
    contact               = "^Public Reporting of Missing Digital Contact Information$",
    crosswalk             = "^Medicare Provider and Supplier Taxonomy Crosswalk$",
    CARE_dialysis         = "^Medicare Dialysis Facilities$",
    enrollees             = "Public Provider Enrollment",
    facilities            = "^Provider of Services File [-] Hospital & Non[-]Hospital Facilities$",
    IQIES                 = "^Provider of Services File [-] Internet Quality Improvement and Evaluation System [-] Home Health Agency, Ambulatory Surgical Center, and Hospice Providers$",
    laboratories          = "^Provider of Services File [-] Clinical Laboratories$",
    long_term             = "^Long[-]Term Care Facility Characteristics$",
    opt_out               = "^Opt Out Affidavits$",
    order_refer           = "^Order and Referring$",
    RBCS                  = "^Restructured BETOS Classification System$",
    transparency          = "^Hospital Price Transparency Enforcement Activities and Outcomes$",
    prescribers_spending  = "^Medicare Part D Spending by Drug$",
    AHRQ_PSI11            = "^Agency for Healthcare Research and Quality \\(AHRQ\\) Patient Safety Indicator 11 \\(PSI[-]11\\) Measure Rates$",
    AIP_spend_plan        = "^Advance Investment Payment Spend Plan$",
    CPC_primary           = "^CPC Initiative [-] Participating Primary Care Practices$",
    CPC_joint             = "^Comprehensive Care for Joint Replacement Model[:] Metropolitan Statistical Areas$",
    BENE_risk_spend       = "^County[-]level Aggregate Expenditure and Risk Score Data on Assignable Beneficiaries$",
    DRA_HAC_measures      = "^Deficit Reduction Act Hospital[-]Acquired Condition Measures$",
    ESRD_group            = "^End-Stage Renal Disease Facility Aggregation Group Performance$",
    MDS_facility          = "^Facility-Level Minimum Data Set Frequency$",
    MDS_total             = "^Minimum Data Set Frequency$",
    FISS_rendering        = "^Fiscal Intermediary Shared System Attending and Rendering$",
    HIT_providers         = "^Home Infusion Therapy Providers$",
    CERT                  = "^Medicare Fee[-]for[-]Service Comprehensive Error Rate Testing$",
    DPP                   = "^Medicare Diabetes Prevention Program$",
    clinician_group       = "^Managing Clinician Aggregation Group Performance$",
    lab_fee_schedule      = "^Medicare Clinical Laboratory Fee Schedule Private Payer Rates and Volumes$",
    partD_prescribe       = "^Medicare Part D Opioid Prescribing Rates [-] by Geography$",

    MKT_cbsa              = "^Market Saturation [&] Utilization Core[-]Based Statistical Areas$",
    MKT_state_cnty        = "^Market Saturation [&] Utilization State[-]County$",
    PTB_discard           = "^Medicare Part B Discarded Drug Units$",
    PTB_spending          = "^Medicare Part B Spending by Drug$",
    BENE_surv_covid       = "^Medicare Current Beneficiary Survey [-] COVID-19 Supplement$",
    BENE_surv_cost        = "^Medicare Current Beneficiary Survey [-] Cost Supplement$",
    BENE_surv_file        = "^Medicare Current Beneficiary Survey [-] Survey File$",
    GEO_MA                = "^Medicare Advantage Geographic Variation [-] National [&] State$",
    GEO_HRR               = "^Medicare Geographic Variation [-] by Hospital Referral Region$",
    GEO_NSC               = "^Medicare Geographic Variation [-] by National, State [&] County$",
    PDP_month             = "^Monthly Prescription Drug Plan Formulary and Pharmacy Network Information$",
    PDP_quarter           = "^Quarterly Prescription Drug Plan Formulary, Pharmacy Network, and Pricing Information$",
    HHA_owners            = "^Home Health Agency All Owners$",
    HHA_cost_report       = "^Home Health Agency Cost Report$",
    HHA_enrollments       = "^Home Health Agency Enrollments$",
    SPICE_owners          = "^Hospice All Owners$",
    SPICE_enrollments     = "^Hospice Enrollments$",
    SPICE_acute           = "^Medicare Post[-]Acute Care and Hospice [-] by Geography [&] Provider",
    hospital_owners       = "^Hospital All Owners$",
    hospital_chow         = "^Hospital Change of Ownership$",
    hospital_chow_owner   = "^Hospital Change of Ownership - Owner Information$",
    hospital_enrollments  = "^Hospital Enrollments$",
    hospital_cost_report  = "^Hospital Provider Cost Report",
    hospital_service_area = "^Hospital Service Area",
    RHC_owners            = "^Rural Health Clinic All Owners$",
    RHC_enrollments       = "^Rural Health Clinic Enrollments$",
    FQHC_owners           = "^Federally Qualified Health Center All Owners$",
    FQHC_enrollments      = "^Federally Qualified Health Center Enrollments$",
    PILAT_non_physicians  = "^Pending Initial Logging and Tracking Non Physicians$",
    PILAT_physicians      = "^Pending Initial Logging and Tracking Physicians$",
    REVAL_group           = "^Revalidation Clinic Group Practice Reassignment$",
    REVAL_due_date        = "^Revalidation Due Date List$",
    REVAL_reassign        = "^Revalidation Reassignment List$",
    SNF_owners            = "^Skilled Nursing Facility All Owners$",
    SNF_chow              = "^Skilled Nursing Facility Change of Ownership$",
    SNF_chow_owner        = "^Skilled Nursing Facility Change of Ownership [-] Owner Information$",
    SNF_cost_report       = "^Skilled Nursing Facility Cost Report$",
    SNF_enrollments       = "^Skilled Nursing Facility Enrollments$",
    CAID_managed          = "^Medicaid Managed Care",
    CAID_opioid           = "^Medicaid Opioid Prescribing Rates [-] by Geography",
    CAID_drug             = "^Medicaid Spending by Drug",

    ACO_REACH_align      = "^ACO REACH Aligned Beneficiaries$",
    ACO_REACH_elig       = "^ACO REACH Eligible Beneficiaries$",
    ACO_REACH_fin        = "^ACO REACH Financial and Quality Results$",
    ACO_REACH_prov       = "^ACO REACH Providers$",
    ACO_REACH_orgs       = "^REACH ACOs$",
    ACO_pioneer          = "^Pioneer ACO Model$",
    ACO_participants     = "^Accountable Care Organization Participants$",
    ACO_SNF              = "^Accountable Care Organization Skilled Nursing Facility Affiliates$",
    ACO_orgs             = "^Accountable Care Organizations$",
    ACO_bene             = "^Number of Accountable Care Organization Assigned Beneficiaries by County$",

    CMS_MA_enroll        = "^CMS Program Statistics [-] Medicare Advantage [&] Other Health Plan Enrollment$",
    CMS_MA_outpatient    = "^CMS Program Statistics [-] Medicare Advantage [-] Outpatient Facility$",
    CMS_MA_other         = "^CMS Program Statistics [-] Medicare Advantage [-] Physician, Non[-]Physician Practitioner [&] Supplier$",
    CMS_MA_inpatient     = "^CMS Program Statistics [-] Medicare Advantage[-]Inpatient Hospital$",
    CMS_MA_SNF           = "^CMS Program Statistics [-] Medicare Advantage[-]Skilled Nursing Facility$",
    CMS_deaths           = "^CMS Program Statistics [-] Medicare Deaths$",
    CMS_HHA              = "^CMS Program Statistics [-] Medicare Home Health Agency$",
    CMS_spice            = "^CMS Program Statistics [-] Medicare Hospice$",
    CMS_inpatient        = "^CMS Program Statistics [-] Medicare Inpatient Hospital$",
    CMS_new_enroll       = "^CMS Program Statistics [-] Medicare Newly Enrolled$",
    CMS_outpatient       = "^CMS Program Statistics [-] Medicare Outpatient Facility$",
    CMS_TOS              = "^CMS Program Statistics [-] Medicare Part A [&] Part B [-] All Types of Service$",
    CMS_partD            = "^CMS Program Statistics [-] Medicare Part D$",
    CMS_partD_enroll     = "^CMS Program Statistics [-] Medicare Part D Enrollment$",
    CMS_other            = "^CMS Program Statistics [-] Medicare Physician, Non[-]Physician Practitioner [&] Supplier$",
    CMS_premiums         = "^CMS Program Statistics [-] Medicare Premiums$",
    CMS_providers        = "^CMS Program Statistics [-] Medicare Providers$",
    CMS_SNF              = "^CMS Program Statistics [-] Medicare Skilled Nursing Facility$",
    CMS_total_enroll     = "^CMS Program Statistics [-] Medicare Total Enrollment$",
    CMS_dual_enroll      = "^CMS Program Statistics [-] Medicare[-]Medicaid Dual Enrollment$",
    CMS_orig_enroll      = "^CMS Program Statistics [-] Original Medicare Enrollment$",

    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )

  if (!exists("catalog")) .catalog <- catalogs()

  res <- select_alias(.catalog$care$main, x)

  if (empty(res))     cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  if (nrow(res) > 1L) cli_abort(c("x" = "> 1 match found for {.val {x}}."), call = call)

  c(res)

}

#' @autoglobal
#' @noRd
select_care_temp <- function(x, call = caller_env()) {

  x <- switch(
    x,
    quality_payment      = "^Quality Payment Program Experience$",
    procedure_summary    = "^Physician[/]Supplier Procedure Summary$",
    dialysis_facilities  = "^Medicare Dialysis Facilities$",
    aco_shared_savings   = "^Performance Year Financial and Quality Results$",
    opioid_treatment     = "^Opioid Treatment Program Providers$",
    hospital_costreport  = "^Hospital Provider Cost Report$",
    SNF_costreport       = "^Skilled Nursing Facility Cost Report$",
    HHA_costreport       = "^Home Health Agency Cost Report$",
    NH_performance       = "^Nursing Home Affiliated Entity Performance Measures$",
    NH_mds_frequency     = "^Minimum Data Set Frequency$",
    NH_mds_facility      = "^Facility[-]Level Minimum Data Set Frequency$",
    IN_geography         = "^Medicare Inpatient Hospitals [-] by Geography and Service$",
    IN_provider          = "^Medicare Inpatient Hospitals [-] by Provider$",
    IN_service           = "^Medicare Inpatient Hospitals [-] by Provider and Service$",
    OUT_geography        = "^Medicare Outpatient Hospitals [-] by Geography and Service$",
    OUT_service          = "^Medicare Outpatient Hospitals [-] by Provider and Service$",
    PRX_geography        = "^Medicare Part D Prescribers [-] by Geography and Drug$",
    PRX_provider         = "^Medicare Part D Prescribers [-] by Provider$",
    PRX_drug             = "^Medicare Part D Prescribers [-] by Provider and Drug$",
    DME_geography        = "^Medicare Durable Medical Equipment, Devices [&] Supplies [-] by Geography and Service$",
    DME_provider         = "^Medicare Durable Medical Equipment, Devices [&] Supplies [-] by Referring Provider$",
    DME_service          = "^Medicare Durable Medical Equipment, Devices [&] Supplies [-] by Referring Provider and Service$",
    DME_supplier         = "^Medicare Durable Medical Equipment, Devices [&] Supplies [-] by Supplier$",
    DME_supplier_service = "^Medicare Durable Medical Equipment, Devices [&] Supplies [-] by Supplier and Service$",
    NH_staff_nonurse     = "^Payroll Based Journal Daily Non[-]Nurse Staffing$",
    NH_staff_nurse       = "^Payroll Based Journal Daily Nurse Staffing$",
    NH_staff_employee    = "^Payroll Based Journal Employee Detail Nursing Home Staffing$",
    UTIL_geography       = "^Medicare Physician [&] Other Practitioners [-] by Geography and Service$",
    UTIL_provider        = "^Medicare Physician [&] Other Practitioners [-] by Provider$",
    UTIL_service         = "^Medicare Physician [&] Other Practitioners [-] by Provider and Service$",
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
