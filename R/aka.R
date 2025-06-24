# ---- care ----
#' @autoglobal
#' @noRd
aka_care <- list(
  endpoint = list(
    ahqr_psi = "^Agency for Healthcare Research and Quality \\(AHRQ\\) Patient Safety Indicator 11 \\(PSI[-]11\\) Measure Rates$",
    aip_plan = "^Advance Investment Payment Spend Plan$",
    cpc_prime = "^CPC Initiative [-] Participating Primary Care Practices$",
    cpc_joint = "^Comprehensive Care for Joint Replacement Model[:] Metropolitan Statistical Areas$",
    county_risk_spend = "^County[-]level Aggregate Expenditure and Risk Score Data on Assignable Beneficiaries$",
    hac_measure = "^Deficit Reduction Act Hospital[-]Acquired Condition Measures$",
    esrd_group = "^End-Stage Renal Disease Facility Aggregation Group Performance$",
    fiss_prov = "^Fiscal Intermediary Shared System Attending and Rendering$",
    home_infusion = "^Home Infusion Therapy Providers$",
    ffs_cert = "^Medicare Fee[-]for[-]Service Comprehensive Error Rate Testing$",
    diabetes_prev = "^Medicare Diabetes Prevention Program$",
    clin_group = "^Managing Clinician Aggregation Group Performance$",
    lab_fee_sched = "^Medicare Clinical Laboratory Fee Schedule Private Payer Rates and Volumes$",
    aco_reach_align = "^ACO REACH Aligned Beneficiaries$",
    aco_reach_elig = "^ACO REACH Eligible Beneficiaries$",
    aco_reach_result = "^ACO REACH Financial and Quality Results$",
    aco_reach_prov = "^ACO REACH Providers$",
    aco_reach_orgs = "^REACH ACOs$",
    aco_pioneer = "^Pioneer ACO Model$",
    aco_parts = "^Accountable Care Organization Participants$",
    aco_snfs = "^Accountable Care Organization Skilled Nursing Facility Affiliates$",
    aco_orgs = "^Accountable Care Organizations$",
    aco_bene_cnty = "^Number of Accountable Care Organization Assigned Beneficiaries by County$",
    care_dial_end = "^Medicare Dialysis Facilities$",
    care_enroll_prov = "Public Provider Enrollment",
    care_pos_fac = "^Provider of Services File [-] Hospital & Non[-]Hospital Facilities$",
    care_pos_iqies = "^Provider of Services File [-] Internet Quality Improvement and Evaluation System [-] Home Health Agency, Ambulatory Surgical Center, and Hospice Providers$",
    care_pos_lab = "^Provider of Services File [-] Clinical Laboratories$",
    care_ltcf = "^Long[-]Term Care Facility Characteristics$",
    care_optout = "^Opt Out Affidavits$",
    care_orderrefer = "^Order and Referring$",
    care_rbetos = "^Restructured BETOS Classification System$",
    care_prmdci = "^Public Reporting of Missing Digital Contact Information$",
    care_tax_xwalk = "^Medicare Provider and Supplier Taxonomy Crosswalk$",
    care_caid_managed = "^Medicaid Managed Care",
    care_caid_opioid = "^Medicaid Opioid Prescribing Rates [-] by Geography",
    care_caid_spend = "^Medicaid Spending by Drug",
    care_enroll_bene = "^Medicare Monthly Enrollment$",
    cms_ma_enroll = "^CMS Program Statistics [-] Medicare Advantage [&] Other Health Plan Enrollment$",
    cms_ma_outpatient = "^CMS Program Statistics [-] Medicare Advantage [-] Outpatient Facility$",
    cms_ma_other = "^CMS Program Statistics [-] Medicare Advantage [-] Physician, Non[-]Physician Practitioner [&] Supplier$",
    cms_ma_inpatient = "^CMS Program Statistics [-] Medicare Advantage[-]Inpatient Hospital$",
    cms_ma_snf = "^CMS Program Statistics [-] Medicare Advantage[-]Skilled Nursing Facility$",
    cms_deaths = "^CMS Program Statistics [-] Medicare Deaths$",
    cms_hha = "^CMS Program Statistics [-] Medicare Home Health Agency$",
    cms_hospice = "^CMS Program Statistics [-] Medicare Hospice$",
    cms_inpatient = "^CMS Program Statistics [-] Medicare Inpatient Hospital$",
    cms_new_enroll = "^CMS Program Statistics [-] Medicare Newly Enrolled$",
    cms_outpatient = "^CMS Program Statistics [-] Medicare Outpatient Facility$",
    cms_tos = "^CMS Program Statistics [-] Medicare Part A [&] Part B [-] All Types of Service$",
    cms_partd = "^CMS Program Statistics [-] Medicare Part D$",
    cms_partd_enroll = "^CMS Program Statistics [-] Medicare Part D Enrollment$",
    cms_phys_npp_supp = "^CMS Program Statistics [-] Medicare Physician, Non[-]Physician Practitioner [&] Supplier$",
    cms_premiums = "^CMS Program Statistics [-] Medicare Premiums$",
    cms_providers = "^CMS Program Statistics [-] Medicare Providers$",
    cms_snf = "^CMS Program Statistics [-] Medicare Skilled Nursing Facility$",
    cms_total_enroll = "^CMS Program Statistics [-] Medicare Total Enrollment$",
    cms_dual_enroll = "^CMS Program Statistics [-] Medicare[-]Medicaid Dual Enrollment$",
    cms_orig_enroll = "^CMS Program Statistics [-] Original Medicare Enrollment$",
    nh_mds_fac_end = "^Facility-Level Minimum Data Set Frequency$",
    nh_mds_end = "^Minimum Data Set Frequency$",
    mkt_cbsa = "^Market Saturation [&] Utilization Core[-]Based Statistical Areas$",
    mkt_state = "^Market Saturation [&] Utilization State[-]County$",
    drug_discard_ptb = "^Medicare Part B Discarded Drug Units$",
    drug_spend_ptb = "^Medicare Part B Spending by Drug$",
    drug_spend_ptd = "^Medicare Part D Spending by Drug$",
    drug_opioid_ptd = "^Medicare Part D Opioid Prescribing Rates [-] by Geography$",
    care_survey_covid = "^Medicare Current Beneficiary Survey [-] COVID-19 Supplement$",
    care_survey_cost = "^Medicare Current Beneficiary Survey [-] Cost Supplement$",
    care_survey = "^Medicare Current Beneficiary Survey [-] Survey File$",
    geo_ma = "^Medicare Advantage Geographic Variation [-] National [&] State$",
    geo_hrr = "^Medicare Geographic Variation [-] by Hospital Referral Region$",
    geo_nsc = "^Medicare Geographic Variation [-] by National, State [&] County$",
    pdp_month = "^Monthly Prescription Drug Plan Formulary and Pharmacy Network Information$",
    pdp_quarter = "^Quarterly Prescription Drug Plan Formulary, Pharmacy Network, and Pricing Information$",
    hha_owner = "^Home Health Agency All Owners$",
    hha_costreport = "^Home Health Agency Cost Report$",
    hha_enroll = "^Home Health Agency Enrollments$",
    spice_owner = "^Hospice All Owners$",
    spice_enroll = "^Hospice Enrollments$",
    spice_acute = "^Medicare Post[-]Acute Care and Hospice [-] by Geography [&] Provider",
    hosp_trans = "^Hospital Price Transparency Enforcement Activities and Outcomes$",
    hosp_owner = "^Hospital All Owners$",
    hosp_chow = "^Hospital Change of Ownership$",
    hosp_chow_owner = "^Hospital Change of Ownership - Owner Information$",
    hosp_enroll = "^Hospital Enrollments$",
    hosp_costreport = "^Hospital Provider Cost Report",
    hosp_service = "^Hospital Service Area",
    rhc_owner = "^Rural Health Clinic All Owners$",
    rhc_enroll = "^Rural Health Clinic Enrollments$",
    fqhc_owner = "^Federally Qualified Health Center All Owners$",
    fqhc_enroll = "^Federally Qualified Health Center Enrollments$",
    pilat_non = "^Pending Initial Logging and Tracking Non Physicians$",
    pilat_phys = "^Pending Initial Logging and Tracking Physicians$",
    reval_group = "^Revalidation Clinic Group Practice Reassignment$",
    reval_due = "^Revalidation Due Date List$",
    reval_list = "^Revalidation Reassignment List$",
    snf_owner = "^Skilled Nursing Facility All Owners$",
    snf_chow = "^Skilled Nursing Facility Change of Ownership$",
    snf_chow_owner = "^Skilled Nursing Facility Change of Ownership [-] Owner Information$",
    snf_costrep_end = "^Skilled Nursing Facility Cost Report$",
    snf_enroll = "^Skilled Nursing Facility Enrollments$"
  ),
  temporal = list(
    quality_payment = "^Quality Payment Program Experience$",
    px_summary = "^Physician[/]Supplier Procedure Summary$",
    care_dialysis_tmp = "^Medicare Dialysis Facilities$",
    aco_shared = "^Performance Year Financial and Quality Results$",
    opioid_treat = "^Opioid Treatment Program Providers$",
    hosp_costrep_tmp = "^Hospital Provider Cost Report$",
    snf_costrep_tmp = "^Skilled Nursing Facility Cost Report$",
    hha_costrep_tmp = "^Home Health Agency Cost Report$",
    nh_perf_tmp = "^Nursing Home Affiliated Entity Performance Measures$",
    nh_mds_freq_tmp = "^Minimum Data Set Frequency$",
    nh_mds_fac_tmp = "^Facility[-]Level Minimum Data Set Frequency$",
    in_geo = "^Medicare Inpatient Hospitals [-] by Geography and Service$",
    in_prov = "^Medicare Inpatient Hospitals [-] by Provider$",
    in_serv = "^Medicare Inpatient Hospitals [-] by Provider and Service$",
    out_geo = "^Medicare Outpatient Hospitals [-] by Geography and Service$",
    out_serv = "^Medicare Outpatient Hospitals [-] by Provider and Service$",
    prx_geo = "^Medicare Part D Prescribers [-] by Geography and Drug$",
    prx_prov = "^Medicare Part D Prescribers [-] by Provider$",
    prx_drug = "^Medicare Part D Prescribers [-] by Provider and Drug$",
    dme_geo = "^Medicare Durable Medical Equipment, Devices [&] Supplies [-] by Geography and Service$",
    dme_prov = "^Medicare Durable Medical Equipment, Devices [&] Supplies [-] by Referring Provider$",
    dme_serv = "^Medicare Durable Medical Equipment, Devices [&] Supplies [-] by Referring Provider and Service$",
    dme_supp = "^Medicare Durable Medical Equipment, Devices [&] Supplies [-] by Supplier$",
    dme_supsrv = "^Medicare Durable Medical Equipment, Devices [&] Supplies [-] by Supplier and Service$",
    pbj_non = "^Payroll Based Journal Daily Non[-]Nurse Staffing$",
    pbj_nurse = "^Payroll Based Journal Daily Nurse Staffing$",
    pbj_emp = "^Payroll Based Journal Employee Detail Nursing Home Staffing$",
    util_geo = "^Medicare Physician [&] Other Practitioners [-] by Geography and Service$",
    util_prov = "^Medicare Physician [&] Other Practitioners [-] by Provider$",
    util_serv = "^Medicare Physician [&] Other Practitioners [-] by Provider and Service$"
  )
)

# ---- caid ----
#' @autoglobal
#' @noRd
aka_caid <- list(
  endpoint = list(
    aca_ful = "ACA Federal Upper Limits",
    caid_drug_rebate = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program",
    caid_enroll_month = "Monthly Enrollment - Test",
    caid_enroll_adult = "Medicaid Enrollment - New Adult Group",
    caid_enterprise = "Medicaid Enterprise System Datatable",
    caid_pi = "PI dataset",
    caid_tiles = "category_tiles",
    mlr_summary = "MLR Summary Reports",
    dsh_require = "Disproportionate Share Hospital \\(DSH\\) Payments [-] Annual Reporting Requirements",
    pharmacy_index = "Division of Pharmacy Releases Index dataset",
    puf_cahps_nam = "NAM CAHPS 2014 Public Use",
    nadac_end = "^NADAC$",
    nadac_first = "^First Time NADAC Rates$",
    nadac_compare = "^NADAC Comparison$",
    caid_fin_mgmt = "Medicaid Financial Management Data$",
    caid_fin_nation = "Medicaid Financial Management Data National Totals",
    caid_chip_bene_month = "Program Information for Medicaid and CHIP Beneficiaries by Month",
    caid_chip_bene_year = "Program Information for Medicaid and CHIP Beneficiaries by Year",
    caid_chip_application = "^CHIP Applications, Eligibility Determinations, and Enrollment Data",
    caid_chip_continue = "^Continuous Eligibility for Medicaid and CHIP Coverage$",
    caid_chip_express = "Express Lane Eligibility for Medicaid and CHIP Coverage",
    caid_chip_metrics = "Medicaid and CHIP CAA Reporting Metrics",
    caid_chip_levels = "Medicaid and CHIP Eligibility Levels",
    caid_chip_renewal = "Medicaid and CHIP Updated Renewal Outcomes",
    caid_chip_presume = "Presumptive Eligibility for Medicaid and CHIP Coverage",
    caid_chip_separate = "Separate CHIP Enrollment by Month and State",
    caid_chip_application_state = "State Medicaid and CHIP Applications, Eligibility Determinations, and Enrollment Data",
    caid_chip_processing_state = "State Medicaid and CHIP Eligibility Processing Data",
    caid_chip_test = "State Medicaid and CHIP Test",
    demo_wellvisit = "^Medicaid and CHIP enrollees who received a well",
    demo_mental = "^Medicaid and CHIP enrollees who received mental health or SUD services$",
    demo_disability = "^Medicaid enrollees who qualify for benefits based on disability$",
    demo_prematurity = "^Prematurity and severe maternal morbidity among Medicaid",
    demo_language = "^Primary language spoken by the Medicaid and CHIP population$",
    demo_ethnicity = "^Race and ethnicity of the national Medicaid and CHIP population$",
    demo_rural = "^Rural Medicaid and CHIP enrollees$",
    demo_waiver = "^Section 1915",
    service_acute = "^Acute Care Services Provided to the Medicaid and CHIP Population$",
    service_behavior = "^Behavioral Health Services\\s?Provided to the Medicaid and CHIP Population$",
    service_perinatal = "^Perinatal Care Services Provided to Medicaid and CHIP Beneficiaries ages 15 to 44",
    service_screening = "^Health Screenings Provided to Medicaid and CHIP Beneficiaries Under Age 19",
    service_contraceptive = "^Contraceptive Care Services Provided to Medicaid and CHIP Beneficiaries ages 15 to 44",
    service_dental = "^Dental Services Provided to Medicaid and CHIP Beneficiaries Under Age 19",
    service_pregnancy = "^Pregnancy Outcomes for Medicaid and CHIP Beneficiaries ages 15 to 44",
    service_telehealth = "^Telehealth Services Provided to the Medicaid and CHIP Population",
    service_vaccination = "^Vaccinations Provided to the Medicaid and CHIP Population under age 19",
    service_bloodlead = "^Blood Lead Screening Services Provided to Medicaid and CHIP Beneficiaries",
    service_respiratory = "^Respiratory Conditions in the Medicaid and CHIP Population",
    benes_behavior = "Beneficiaries receiving a behavioral health service by behavioral health condition, 2017-2021",
    benes_physical = "Beneficiaries receiving a physical health service among beneficiaries receiving a SUD service by physical health cond, 2017-2021",
    benes_mental = "Beneficiaries receiving a physical hlth serv among beneficiaries receiving a mental hlth serv, by physical hlth cond, 2017-2021",
    benes_integrated = "Beneficiaries who could benefit from integrated care, 2017-2021",
    benes_nas = "Number and rate of NAS per 1,000 births in newborns whose deliveries were covered by Medicaid or CHIP, 2017 - 2021",
    benes_smm = "Number and rate of SMM among Medicaid- and CHIP-covered deliveries, 2017 - 2021",
    benes_pregnant = "Number of pregnant and postpartum Medicaid and CHIP beneficiaries, 2017-2021",
    benes_delivery = "Rate of NAS per 1,000 births in newborns whose deliveries were covered by Medicaid or CHIP, 2017 - 2019",
    benefit_pkg_month = "Benefit Package for Medicaid and CHIP Beneficiaries by Month",
    benefit_pkg_year = "Benefit Package for Medicaid and CHIP Beneficiaries by Year",
    drug_amp_month = "Drug AMP Reporting - Monthly",
    drug_amp_quarter = "Drug AMP Reporting - Quarterly",
    drug_products = "Drug Products in the Medicaid Drug Rebate Program",
    drug_clot = "^Clotting Factor Drug Report",
    drug_peds = "Exclusive Pediatric Drugs",
    drug_contact_man = "Drug Manufacturer Contacts",
    drug_contact_state = "Medicaid Drug Rebate Program State Contact Information",
    dual_status_month = "Dual Status Information for Medicaid and CHIP Beneficiaries by Month",
    dual_status_year = "Dual Status Information for Medicaid and CHIP Beneficiaries by Year",
    meg_month = "Major Eligibility Group Information for Medicaid and CHIP Beneficiaries by Month",
    meg_year = "Major Eligibility Group Information for Medicaid and CHIP Beneficiaries by Year",
    cms64_caa = "Medicaid CMS-64 CAA 2023 Increased FMAP Expenditure Data Collected through MBES/CBES",
    cms64_ffcra = "Medicaid CMS-64 FFCRA Increased FMAP Expenditure",
    cms64_adult = "Medicaid CMS-64 New Adult Group Expenditures",
    managed_mltss = "Managed Long Term Services and Supports \\(MLTSS\\) Enrollees",
    managed_summary = "Managed Care Enrollment Summary",
    managed_state = "^Managed Care Programs\\sby\\sState$",
    managed_program = "Managed Care Enrollment by Program and Plan",
    managed_pop = "Managed Care Enrollment by Program and Population \\(All\\)",
    managed_dual = "Managed Care Enrollment by Program and Population \\(Duals\\)",
    managed_feat_pop = "Managed Care Features By Enrollment Population",
    managed_feat_qa = "Managed Care Features by QA and Performance Incentive",
    managed_bene_month = "Managed Care Information for Medicaid and CHIP Beneficiaries by Month",
    managed_bene_year = "Managed Care Information for Medicaid and CHIP Beneficiaries by Year",
    managed_share = "Share of Medicaid Enrollees in Managed Care",
    unwind_market = "HealthCare\\.gov Marketplace Medicaid Unwinding Report",
    unwind_transition = "HealthCare\\.gov Transitions Marketplace Medicaid Unwinding Report",
    unwind_historic = "Separate CHIP Enrollment by Month and State\\sHistoric CAA/Unwinding Period",
    unwind_sbm = "State-based Marketplace \\(SBM\\) Medicaid Unwinding Report"
  ),
  temporal = list(
    nadac_tmp = "^NADAC$",
    managed_by_state = "^Managed Care Programs by State$",
    drug_rebate_week = "^Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program$",
    blood_disorder = "^Pricing Comparison for Blood Disorder Treatments$",
    state_drug = "^State Drug Utilization Data$",
    hc_quality = "^Child and Adult Health Care Quality Measures$"
  )
)

# ---- open ----
#' @autoglobal
#' @noRd
aka_open <- list(
  endpoint = list(
    profile_covered = "^Covered Recipient Profile Supplement$",
    profile_physician = "^Physician \\(Distinct\\) Profile Information$",
    profile_information = "^Profile Information$",
    profile_mapping = "^Provider Profile ID Mapping Table$",
    profile_entity = "^Reporting Entity Profile Information$",
    profile_teaching = "^Teaching Hospital Profile Information$",
    summary_dashboard = "^Summary Dashboard",
    summary_state = "^State Level Payment Total and Averages for all Years$",
    summary_nature = "^State Payment Totals and Averages Grouped by Nature of Payment for all Years$",
    summary_national = "^National Level Payment Total and Averages for all Years$",
    summary_specialty = "^National Level Payment Total and Averages by Provider Specialty for all Years$"
  ),
  temporal = list(
    payment_general = "^General Payment Data$",
    payment_ownership = "^Ownership Payment Data$",
    payment_research = "^Research Payment Data$",
    grouped_covered_nature = "^Payments Grouped by Covered Recipient and Nature of Payments$",
    grouped_covered_entity = "^Payments Grouped by Covered Recipient and Reporting Entities$",
    grouped_entity_nature = "^Payments Grouped by Reporting Entities and Nature of Payments$",
    grouped_entity_covered_nature = "^Payments Grouped by Reporting Entities, Covered Recipient, and Nature of Payments$",
    grouped_state_nature = "^State Payment Totals Grouped by Nature of Payment for all Years$"
  )
)

# ---- prov ----
#' @autoglobal
#' @noRd
aka_prov <- list(
  endpoint = list(
    asc_facility = "^Ambulatory Surgical Center Quality Measures [-] Facility",
    asc_national = "^Ambulatory Surgical Center Quality Measures [-] National",
    asc_state = "^Ambulatory Surgical Center Quality Measures [-] State",
    cahps_hospice_nation = "^Hospice care [-] National CAHPS Hospice Survey Data$",
    cahps_hospice_provider = "^Hospice care [-] Provider CAHPS Hospice Survey Data$",
    cahps_hospice_state = "^Hospice care [-] State CAHPS Hospice Survey Data$",
    cahps_hhc_patient = "^Home Health Care [-] Patient Survey \\(HHCAHPS\\) 2023Q4 to 2024Q3$",
    cahps_hhc_measure = "^Home Health Care [-] Patient Survey \\(HHCAHPS\\) Measure Dates 2023Q4 to 2024Q3$",
    cahps_hhc_national = "^Home Health Care [-] Patient Survey \\(HHCAHPS\\) National Data 2023Q4 to 2024Q3$",
    cahps_hhc_state = "^Home Health Care [-] Patient Survey \\(HHCAHPS\\) State Data 2023Q4 to 2024Q3$",
    cahps_ich_esrd = "^ESRD QIP [-] In[-]Center Hemodialysis Consumer Assessment Of Healthcare Providers And Services Systems \\(ICH CAHPS\\) Survey",
    cahps_ich_facility = "^Patient survey \\(ICH CAHPS\\) [-] Facility$",
    cahps_ich_national = "^Patient survey \\(ICH CAHPS\\) [-] National$",
    cahps_ich_state = "^Patient survey \\(ICH CAHPS\\) [-] State$",
    cahps_oas_footnotes = "^Outpatient and Ambulatory Surgery Consumer Assessment of Healthcare Providers and Systems \\(OAS CAHPS\\) survey [-] Footnotes",
    cahps_oas_asc_facility = "^Outpatient and Ambulatory Surgery Consumer Assessment of Healthcare Providers and Systems \\(OAS CAHPS\\) survey for ambulatory surgical centers [-] Facility",
    cahps_oas_asc_national = "^Outpatient and Ambulatory Surgery Consumer Assessment of Healthcare Providers and Systems \\(OAS CAHPS\\) survey for ambulatory surgical centers [-] National",
    cahps_oas_asc_state = "^Outpatient and Ambulatory Surgery Consumer Assessment of Healthcare Providers and Systems \\(OAS CAHPS\\) survey for ambulatory surgical centers [-] State",
    cahps_oas_hosp_facility = "^Outpatient and Ambulatory Surgery Consumer Assessment of Healthcare Providers and Systems \\(OAS CAHPS\\) survey for hospital outpatient departments [-] Facility",
    cahps_oas_hosp_national = "^Outpatient and Ambulatory Surgery Consumer Assessment of Healthcare Providers and Systems \\(OAS CAHPS\\) survey for hospital outpatient departments [-] National",
    cahps_oas_hosp_state = "^Outpatient and Ambulatory Surgery Consumer Assessment of Healthcare Providers and Systems \\(OAS CAHPS\\) survey for hospital outpatient departments [-] State",
    hcahps_pch_hospital = "Patient Survey \\(PCH [-] HCAHPS\\) PPS[-]Exempt Cancer Hospital [-] Hospital",
    hcahps_pch_national = "Patient Survey \\(PCH [-] HCAHPS\\) PPS[-]Exempt Cancer Hospital [-] National",
    hcahps_pch_state = "Patient Survey \\(PCH [-] HCAHPS\\) PPS[-]Exempt Cancer Hospital [-] State",
    hcahps_hospital = "Patient survey \\(HCAHPS\\) [-] Hospital",
    hcahps_national = "Patient survey \\(HCAHPS\\) [-] National",
    hcahps_state = "Patient survey \\(HCAHPS\\) [-] State",
    complication_hospital = "^Complications and Deaths [-] Hospital",
    complication_national = "^Complications and Deaths [-] National",
    complication_state = "^Complications and Deaths [-] State",
    complication_pch_hospital = "^Complications and Unplanned Hospital Visits [-] PPS-Exempt Cancer Hospital [-] Hospital",
    complication_pch_national = "^Complications and Unplanned Hospital Visits [-] PPS-Exempt Cancer Hospital [-] National",
    dialysis_by_facility = "^Dialysis Facility [-] Listing by Facility$",
    dialysis_national = "^Dialysis Facility [-] National Averages$",
    dialysis_state = "^Dialysis Facility [-] State Averages$",
    esrd_depression = "^ESRD QIP [-] Clinical Depression Screening and Follow-up",
    esrd_complete = "^ESRD QIP [-] Complete QIP Data",
    esrd_adequacy = "^ESRD QIP [-] Dialysis Adequacy",
    esrd_footnotes = "^ESRD QIP [-] Footnotes",
    esrd_hypercalcemia = "^ESRD QIP [-] Hypercalcemia",
    esrd_medication = "^ESRD QIP [-] Medication Reconciliation",
    esrd_infection = "^ESRD QIP [-] NHSN Bloodstream Infection",
    esrd_event = "^ESRD QIP [-] NHSN Dialysis Event Measure",
    esrd_waitlist = "^ESRD QIP [-] Percentage of Prevalent Patients Waitlisted",
    esrd_hospitalization = "^ESRD QIP [-] Standardized Hospitalization Ratio",
    esrd_readmission = "^ESRD QIP [-] Standardized Readmission Ratio",
    esrd_transfusion = "^ESRD QIP [-] Standardized Transfusion Ratio",
    esrd_performance = "^ESRD QIP [-] Total Performance Scores",
    esrd_ultrafiltration = "^ESRD QIP [-] Ultrafiltration Rate",
    esrd_vascular = "^ESRD QIP [-] Vascular Access Topic",
    hai_hospital = "^Healthcare Associated Infections [-] Hospital",
    hai_national = "^Healthcare Associated Infections [-] National",
    hai_state = "^Healthcare Associated Infections [-] State",
    hai_pch = "^Safety and Healthcare[-]Associated Infection Measures [-] PPS[-]Exempt Cancer Hospital",
    he_hospital = "^Health Equity [-] Hospital",
    he_national = "^Health Equity [-] National",
    he_state = "^Health Equity [-] State",
    hhc_range = "^Home Health Care [-] Measure Date Range$",
    hhc_national = "^Home Health Care [-] National Data$",
    hhc_state = "^Home Health Care [-] State by State Data$",
    hhc_zip = "^Home Health Care [-] Zip Codes$",
    hhc_agency = "^Home Health Care Agencies$",
    hhvbp_agency = "^Expanded Home Health Value-Based Purchasing \\(HHVBP\\) Model [-] Agency Data$",
    hhvbp_cohort = "^Expanded Home Health Value-Based Purchasing \\(HHVBP\\) Model [-] Cohort Data$",
    hvbp_outcomes = "^Hospital Value[-]Based Purchasing \\(HVBP\\) [-] Clinical Outcomes Domain Scores",
    hvbp_efficiency = "^Hospital Value[-]Based Purchasing \\(HVBP\\) [-] Efficiency Scores",
    hvbp_engagement = "^Hospital Value[-]Based Purchasing \\(HVBP\\) [-] Person and Community Engagement Domain Scores \\(HCAHPS\\)",
    hvbp_safety = "^Hospital Value[-]Based Purchasing \\(HVBP\\) [-] Safety",
    hvbp_performance = "^Hospital Value[-]Based Purchasing \\(HVBP\\) [-] Total Performance Score",
    hospice_general = "^Hospice [-] General Information$",
    hospice_provider = "^Hospice [-] Provider Data$",
    hospice_state = "^Hospice [-] State Data$",
    hospice_zip = "^Hospice [-] Zip Data$",
    hospice_national = "^Hospice[-]National Data$",
    hospital_psi90 = "^CMS Medicare PSI[-]90 and component measures [-] six[-]digit estimate dataset",
    hospital_joint = "^Comprehensive Care For Joint Replacement Model [-] Provider Data",
    hospital_footnote = "^Footnote Crosswalk",
    hospital_update = "^Data Updates",
    hospital_dates = "^Measure Dates",
    hospital_maternal = "^Maternal Health [-] Hospital",
    hospital_outcomes = "^Patient[-]Reported Outcomes [-] Hospital",
    hospital_general = "^Hospital General Information",
    hospital_pi = "^Promoting Interoperability [-] Hospital",
    hospital_voc_nation = "^Value of care [-] National",
    hospital_voc_hosp = "^Payment and value of care [-] Hospital",
    hospital_pmt_state = "^Payment [-] State",
    hospital_pmt_nation = "^Payment [-] National",
    hospital_drg_net = "^Table 1[:] FY2021 Net Change in Base Operating DRG Payment Amount",
    hospital_drg_dist = "^Table 2[:] FY2021 Distribution of Net Change in Base Operating DRG Payment Amount",
    hospital_pmt_pct = "^Table 3[:] FY2021 Percent Change in Medicare Payments",
    hospital_pmt_vbi = "^Table 4[:] FY2021 Value-Based Incentive Payment Amount",
    ipf_national = "^Inpatient Psychiatric Facility Quality Measure Data [-] National",
    ipf_facility = "^Inpatient Psychiatric Facility Quality Measure Data [-] by Facility",
    ipf_state = "^Inpatient Psychiatric Facility Quality Measure Data [-] by State",
    irf_conditions = "^Inpatient Rehabilitation Facility [-] Conditions$",
    irf_general = "^Inpatient Rehabilitation Facility [-] General Information$",
    irf_provider = "^Inpatient Rehabilitation Facility [-] Provider Data$",
    irf_national = "^Inpatient Rehabilitation Facility [-] National Data$",
    ltch_general = "^Long-Term Care Hospital [-] General Information$",
    ltch_provider = "^Long-Term Care Hospital [-] Provider Data$",
    ltch_national = "^Long-Term Care Hospital [-] National Data$",
    mips_performance = "^PY 2022 Clinician Public Reporting[:] Overall MIPS Performance$",
    mips_patient = "^PY 2022 Group Public Reporting[:] Patient Experience$",
    mips_clinician = "^PY 2022 Clinician Public Reporting[:] MIPS Measures and Attestations$",
    mips_group = "^PY 2022 Group Public Reporting[:] MIPS Measures and Attestations$",
    mips_virtual = "^PY 2022 Virtual Group Public Reporting[:] MIPS Measures and Attestations$",
    mspb_claim = "^Medicare Hospital Spending by Claim",
    mspb_hospital = "^Medicare Spending Per Beneficiary [-] Hospital$",
    mspb_decimal = "^Medicare Spending Per Beneficiary [-] Hospital Additional Decimal Places$",
    mspb_national = "^Medicare Spending Per Beneficiary [-] National$",
    mspb_state = "^Medicare Spending Per Beneficiary [-] State$",
    nursing_ownership = "^Ownership$",
    nursing_penalties = "^Penalties$",
    nursing_provider = "^Provider Information$",
    nursing_citation = "^Citation Code Look[-]up$",
    nursing_fire = "^Fire Safety Deficiencies$",
    nursing_deficiencies = "^Health Deficiencies$",
    nursing_inspection = "^Inspection Dates$",
    nursing_quality_mds = "^MDS Quality Measures$",
    nursing_quality_claims = "^Medicare Claims Quality Measures$",
    nursing_state_avg = "^State US Averages$",
    nursing_state_cut = "^State-Level Health Inspection Cut Points$",
    nursing_interval = "^Nursing Home Data Collection Interval",
    out_img_hospital = "^Outpatient Imaging Efficiency [-] Hospital",
    out_img_national = "^Outpatient Imaging Efficiency [-] National",
    out_img_state = "^Outpatient Imaging Efficiency [-] State",
    pch_pall_hospital = "^Palliative Care [-] PPS-Exempt Cancer Hospital [-] Hospital",
    pch_pall_national = "^Palliative Care [-] PPS-Exempt Cancer Hospital [-] National",
    pdc_affiliations = "^Facility Affiliation Data$",
    pdc_clinicians = "^National Downloadable File$",
    pdc_utilization = "^Utilization Data$",
    reduction_hac = "Hospital[-]Acquired Condition \\(HAC\\) Reduction Program",
    reduction_hrr = "Hospital Readmissions Reduction Program",
    snf_vbp_performance = "^FY 2025 SNF VBP Aggregate Performance$",
    snf_vbp_facility = "^FY 2025 SNF VBP Facility[-]Level Dataset$",
    snf_quality_nation = "^Skilled Nursing Facility Quality Reporting Program [-] National Data$",
    snf_quality_provider = "^Skilled Nursing Facility Quality Reporting Program [-] Provider Data$",
    snf_quality_swing = "^Skilled Nursing Facility Quality Reporting Program [-] Swing Beds [-] Provider Data$",
    supplier_directory = "^Medical Equipment Suppliers$",
    timely_hospital = "^Timely and Effective Care [-] Hospital",
    timely_national = "^Timely and Effective Care [-] National",
    timely_state = "^Timely and Effective Care [-] State",
    unplan_hospital = "^Unplanned Hospital Visits [-] Hospital",
    unplan_national = "^Unplanned Hospital Visits [-] National",
    unplan_state = "^Unplanned Hospital Visits [-] State",
    va_behavioral = "^Veterans Health Administration Behavioral Health Data",
    va_provider = "^Veterans Health Administration Provider Level Data",
    va_timely = "^Veterans Health Administration Timely and Effective Care Data"
  )
)

# ---- hgov ----
#' @autoglobal
#' @noRd
aka_hgov <- list(
  endpoint = list(
    hgov_auto_pop           = "^Auto[-]population File$",
    hgov_ab_reg_comp        = "^AB Registration Completion List$",
    hgov_ab_sus_term        = "^AB Suspension and Termination List$",
    hgov_ab_reg_gloss       = "^Agent Broker Registration Tracker Glossary$",
    hgov_ab_reg_trac        = "^Agent Broker Registration Tracker$",
    hgov_catastrophic       = "^Catastrophic Plans for People with Cancelled Policies$",
    hgov_contact_admin      = "^Helpful Contacts Admins$",
    hgov_counties           = "^Counties$",
    hgov_county_service     = "^County Service Areas$",
    hgov_partner_lookup     = "^Issuer[_]Partner[_]Lookup$",
    hgov_partner_reference  = "^Issuer Partner Directory [-] Reference Text$",
    hgov_partner_directory  = "^Issuer and DE Partner Directory$",
    hgov_partner_enrollment = "^Direct Enrollment Partners$",
    hgov_nipr_authority     = "^NIPR Valid Lines of Authority List$",
    hgov_nipr_state         = "^Marketplace Agent[/]Broker NIPR Valid Lines of Authority[,] by State$",
    hgov_response_codes     = "^Response Codes$",
    hgov_rolling_draft      = "^Rolling Draft ECP List$",
    hgov_slcsp_county       = "^SLCSP [-] County[-]Zip Reference Data$",
    hgov_states             = "^States$",
    hgov_local_help         = "^Find Local Help$",
    hgov_qhp_consumer       = "QHP Selections by Type of Consumer and County",
    hgov_qhp_aptc           = "QHP Selections by APTC and County",
    hgov_qhp_csr            = "QHP Selections by CSR and County",
    hgov_qhp_metal          = "QHP Selections by Metal Level and County",
    hgov_qhp_income         = "QHP Selections by Household Income as a Percent of the Federal Poverty Level and County",
    hgov_qhp_ethnicity      = "QHP Selections by Race/Ethnicity and County",
    hgov_qhp_age            = "QHP Selections by Age Group and County",
    hgov_qhp_business       = "QHP Landscape Health Plan Business Rule Variables"
  ),
  temporal = list(
    hgov_mlr             = "^MLR Dataset$",
    hgov_puf_benefits    = "^Benefits and Cost Sharing PUF$",
    hgov_puf_business    = "^Business Rules PUF$",
    hgov_puf_machine     = "^Machine Readable PUF$",
    hgov_puf_network     = "^Network PUF$",
    hgov_puf_plan_attr   = "^Plan Attributes PUF$",
    hgov_puf_plan_walk   = "^Plan ID Crosswalk PUF$",
    hgov_puf_rate        = "^Rate PUF$",
    hgov_puf_service     = "^Service Area PUF$",
    hgov_puf_tic         = "^Transparency in Coverage PUF$",
    # THESE CONTAIN ZIP FILES NOT ENDPOINTS
    hgov_qhp_ind_dnt     = "QHP Landscape Individual Market Dental",
    hgov_qhp_ind_med     = "QHP Landscape Individual Market Medical",
    hgov_qhp_shop_dnt    = "QHP Landscape SHOP Market Dental",
    hgov_qhp_shop_med    = "QHP Landscape SHOP Market Medical"
  )
)

# ---- aka_names ----
#' @autoglobal
#' @noRd
aka_names <- mph_init(
  names(c(
    aka_prov$endpoint,
    aka_hgov$endpoint,
    aka_caid$endpoint,
    aka_care$endpoint,
    aka_open$endpoint,
    aka_hgov$temporal,
    aka_caid$temporal,
    aka_care$temporal,
    aka_open$temporal
  )))

# ---- aka_regex ----
#' @autoglobal
#' @noRd
aka_regex <- unlist(
    c(
      aka_prov$endpoint,
      aka_hgov$endpoint,
      aka_caid$endpoint,
      aka_care$endpoint,
      aka_open$endpoint,
      aka_hgov$temporal,
      aka_caid$temporal,
      aka_care$temporal,
      aka_open$temporal
    ),
    use.names = FALSE
  )

#' @autoglobal
#' @noRd
alias_regex <- function(x) {
  aka_regex[mph_match(x, aka_names)]
}

# ---- apitype ----
#' @autoglobal
#' @noRd
apitype <- list(
  endpoint = mph_init(names(
    c(
      aka_prov$endpoint,
      aka_hgov$endpoint,
      aka_caid$endpoint,
      aka_care$endpoint,
      aka_open$endpoint
    )
  )),
  temporal = mph_init(names(
    c(
      aka_hgov$temporal,
      aka_caid$temporal,
      aka_care$temporal,
      aka_open$temporal
    )
  ))
)

# ---- clogtype ----
#' @autoglobal
#' @noRd
clogtype <- list(
  care = mph_init(names(
    c(aka_care$endpoint,
      aka_care$temporal)
  )),
  prov = mph_init(names(
    c(aka_prov$endpoint)
  )),
  open = mph_init(names(
    c(aka_open$endpoint,
      aka_open$temporal)
  )),
  caid = mph_init(names(
    c(aka_caid$endpoint,
      aka_caid$temporal)
  )),
  hgov = mph_init(names(c(
    aka_hgov$endpoint,
    aka_hgov$temporal
  )))
)

#' @autoglobal
#' @noRd
catalog_type <- function(x) {
  x <- nif(
    is_clog_care(x), "care",
    is_clog_caid(x), "caid",
    is_clog_prov(x), "prov",
    is_clog_open(x), "open",
    is_clog_hgov(x), "hgov",
    default = NA_character_)

  if (is.na(x)) cli_abort(
    c("x" = "{.val {x}} is invalid."), call = call)

  x
}

#' @autoglobal
#' @noRd
api_type <- function(x) {
  x <- nif(
    is_api_endpoint(x), "end",
    is_api_temporal(x), "tmp",
    default = NA_character_)

  if (is.na(x)) cli_abort(
    c("x" = "{.val {x}} is invalid."), call = call)

  x
}

#' @autoglobal
#' @noRd
is_clog_care <- function(x) {
  !is.na(mph_match(x, clogtype$care))
}

#' @autoglobal
#' @noRd
is_clog_caid <- function(x) {
  !is.na(mph_match(x, clogtype$caid))
}

#' @autoglobal
#' @noRd
is_clog_prov <- function(x) {
  !is.na(mph_match(x, clogtype$prov))
}

#' @autoglobal
#' @noRd
is_clog_open <- function(x) {
  !is.na(mph_match(x, clogtype$open))
}

#' @autoglobal
#' @noRd
is_clog_hgov <- function(x) {
  !is.na(mph_match(x, clogtype$hgov))
}

#' @autoglobal
#' @noRd
is_api_endpoint <- function(x) {
  !is.na(mph_match(x, apitype$endpoint))
}

#' @autoglobal
#' @noRd
is_api_temporal <- function(x) {
  !is.na(mph_match(x, apitype$temporal))
}

#' @autoglobal
#' @noRd
is_collection_api <- function(x) {
  !is.na(mph_match(x, apitype$collection))
}

