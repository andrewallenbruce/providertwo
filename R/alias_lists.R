#' @autoglobal
#' @noRd
care_list <- list(
  endpoint = list(
    contact              = "Public Reporting of Missing Digital Contact Information",
    crosswalk            = "Medicare Provider and Supplier Taxonomy Crosswalk",
    CARE_dialysis        = "Medicare Dialysis Facilities",
    enrollees            = "Public Provider Enrollment",
    facilities           = "Provider of Services File - Hospital & Non-Hospital Facilities",
    IQIES                = "Provider of Services File - Internet Quality Improvement and Evaluation System - Home Health Agency, Ambulatory Surgical Center, and Hospice Providers",
    laboratories         = "Provider of Services File - Clinical Laboratories",
    long_term            = "Long-Term Care Facility Characteristics",
    opt_out              = "Opt Out Affidavits",
    order_refer          = "Order and Referring",
    RBCS                 = "Restructured BETOS Classification System",
    transparency         = "Hospital Price Transparency Enforcement Activities and Outcomes",
    HHA_owners           = "^Home Health Agency All Owners$",
    HHA_cost_report      = "^Home Health Agency Cost Report$",
    HHA_enrollments      = "^Home Health Agency Enrollments$",
    hospice_owners       = "^Hospice All Owners$",
    hospice_enrollments  = "^Hospice Enrollments$",
    hospice_acute        = "Medicare Post-Acute Care and Hospice - by Geography & Provider",
    hospital_owners      = "^Hospital All Owners$",
    hospital_chow        = "^Hospital Change of Ownership$",
    hospital_chow_owner  = "^Hospital Change of Ownership - Owner Information$",
    hospital_enrollments = "^Hospital Enrollments$",
    RHC_owners           = "^Rural Health Clinic All Owners$",
    RHC_enrollments      = "^Rural Health Clinic Enrollments$",
    FQHC_owners          = "^Federally Qualified Health Center All Owners$",
    FQHC_enrollments     = "^Federally Qualified Health Center Enrollments$",
    PILAT_non_physicians = "^Pending Initial Logging and Tracking Non Physicians$",
    PILAT_physicians     = "^Pending Initial Logging and Tracking Physicians$",
    REVAL_group          = "^Revalidation Clinic Group Practice Reassignment$",
    REVAL_due_date       = "^Revalidation Due Date List$",
    REVAL_reassignment   = "^Revalidation Reassignment List$",
    SNF_owners           = "^Skilled Nursing Facility All Owners$",
    SNF_chow             = "^Skilled Nursing Facility Change of Ownership$",
    SNF_chow_owner       = "^Skilled Nursing Facility Change of Ownership - Owner Information$",
    SNF_cost_report      = "^Skilled Nursing Facility Cost Report$",
    SNF_enrollments      = "^Skilled Nursing Facility Enrollments$"
  ),
  temporal = list(
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
    UTIL_provider_and_service  = "^Medicare Physician & Other Practitioners - by Provider and Service$"
  )
)

#' @autoglobal
#' @noRd
caid_list <- list(
  endpoint = list(
    aca_ful                       = "ACA Federal Upper Limits",
    caid_drug_rebate              = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program",
    caid_enroll_month             = "Monthly Enrollment - Test",
    caid_enroll_adult             = "Medicaid Enrollment - New Adult Group",
    caid_enterprise               = "Medicaid Enterprise System Datatable",
    caid_pi                       = "PI dataset",
    caid_tiles                    = "category_tiles",
    mlr_summary                   = "MLR Summary Reports",
    managed_longterm              = "Managed Long Term Services and Supports \\(MLTSS\\) Enrollees",
    dsh_requirement               = "Disproportionate Share Hospital \\(DSH\\) Payments [-] Annual Reporting Requirements",
    pharm_release_index           = "Division of Pharmacy Releases Index dataset",
    cahps_nam_puf                 = "NAM CAHPS 2014 Public Use",
    caid_nadac                    = "^NADAC$",
    caid_nadac_first              = "^First Time NADAC Rates$",
    caid_nadac_compare            = "^NADAC Comparison$",
    caid_finance_mgmt             = "Medicaid Financial Management Data$",
    caid_finance_nation           = "Medicaid Financial Management Data National Totals",
    caid_chip_bene_month          = "Program Information for Medicaid and CHIP Beneficiaries by Month",
    caid_chip_bene_year           = "Program Information for Medicaid and CHIP Beneficiaries by Year",
    caid_chip_application         = "^CHIP Applications, Eligibility Determinations, and Enrollment Data",
    caid_chip_continue            = "^Continuous Eligibility for Medicaid and CHIP Coverage$",
    caid_chip_express             = "Express Lane Eligibility for Medicaid and CHIP Coverage",
    caid_chip_metrics             = "Medicaid and CHIP CAA Reporting Metrics",
    caid_chip_levels              = "Medicaid and CHIP Eligibility Levels",
    caid_chip_renewal             = "Medicaid and CHIP Updated Renewal Outcomes",
    caid_chip_presume             = "Presumptive Eligibility for Medicaid and CHIP Coverage",
    caid_chip_separate            = "Separate CHIP Enrollment by Month and State",
    caid_chip_application_state   = "State Medicaid and CHIP Applications, Eligibility Determinations, and Enrollment Data",
    caid_chip_processing_state    = "State Medicaid and CHIP Eligibility Processing Data",
    caid_chip_test                = "State Medicaid and CHIP Test",
    demo_wellvisit                = "^Medicaid and CHIP enrollees who received a well",
    demo_mental                   = "^Medicaid and CHIP enrollees who received mental health or SUD services$",
    demo_disability               = "^Medicaid enrollees who qualify for benefits based on disability$",
    demo_prematurity              = "^Prematurity and severe maternal morbidity among Medicaid",
    demo_language                 = "^Primary language spoken by the Medicaid and CHIP population$",
    demo_ethnicity                = "^Race and ethnicity of the national Medicaid and CHIP population$",
    demo_rural                    = "^Rural Medicaid and CHIP enrollees$",
    demo_waiver                   = "^Section 1915",
    service_acute                 = "^Acute Care Services Provided to the Medicaid and CHIP Population$",
    service_behavior              = "^Behavioral Health Services\\s?Provided to the Medicaid and CHIP Population$",
    service_perinatal             = "^Perinatal Care Services Provided to Medicaid and CHIP Beneficiaries ages 15 to 44",
    service_screening             = "^Health Screenings Provided to Medicaid and CHIP Beneficiaries Under Age 19",
    service_contraceptive         = "^Contraceptive Care Services Provided to Medicaid and CHIP Beneficiaries ages 15 to 44",
    service_dental                = "^Dental Services Provided to Medicaid and CHIP Beneficiaries Under Age 19",
    service_pregnancy             = "^Pregnancy Outcomes for Medicaid and CHIP Beneficiaries ages 15 to 44",
    service_telehealth            = "^Telehealth Services Provided to the Medicaid and CHIP Population",
    service_vaccination           = "^Vaccinations Provided to the Medicaid and CHIP Population under age 19",
    service_bloodlead             = "^Blood Lead Screening Services Provided to Medicaid and CHIP Beneficiaries",
    service_respiratory           = "^Respiratory Conditions in the Medicaid and CHIP Population",
    benes_behavior                = "Beneficiaries receiving a behavioral health service by behavioral health condition, 2017-2021",
    benes_physical                = "Beneficiaries receiving a physical health service among beneficiaries receiving a SUD service by physical health cond, 2017-2021",
    benes_mental                  = "Beneficiaries receiving a physical hlth serv among beneficiaries receiving a mental hlth serv, by physical hlth cond, 2017-2021",
    benes_integrated              = "Beneficiaries who could benefit from integrated care, 2017-2021",
    benes_nas                     = "Number and rate of NAS per 1,000 births in newborns whose deliveries were covered by Medicaid or CHIP, 2017 - 2021",
    benes_smm                     = "Number and rate of SMM among Medicaid- and CHIP-covered deliveries, 2017 - 2021",
    benes_pregnant                = "Number of pregnant and postpartum Medicaid and CHIP beneficiaries, 2017-2021",
    benes_delivery                = "Rate of NAS per 1,000 births in newborns whose deliveries were covered by Medicaid or CHIP, 2017 - 2019",
    benefit_pkg_month             = "Benefit Package for Medicaid and CHIP Beneficiaries by Month",
    benefit_pkg_year              = "Benefit Package for Medicaid and CHIP Beneficiaries by Year",
    drug_amp_month                = "Drug AMP Reporting - Monthly",
    drug_amp_quarter              = "Drug AMP Reporting - Quarterly",
    drug_products                 = "Drug Products in the Medicaid Drug Rebate Program",
    drug_clot                     = "^Clotting Factor Drug Report",
    drug_pediatric                = "Exclusive Pediatric Drugs",
    drug_contact_manu             = "Drug Manufacturer Contacts",
    drug_contact_state            = "Medicaid Drug Rebate Program State Contact Information",
    dual_status_month             = "Dual Status Information for Medicaid and CHIP Beneficiaries by Month",
    dual_status_year              = "Dual Status Information for Medicaid and CHIP Beneficiaries by Year",
    meg_month                     = "Major Eligibility Group Information for Medicaid and CHIP Beneficiaries by Month",
    meg_year                      = "Major Eligibility Group Information for Medicaid and CHIP Beneficiaries by Year",
    cms64_caa                     = "Medicaid CMS-64 CAA 2023 Increased FMAP Expenditure Data Collected through MBES/CBES",
    cms64_ffcra                   = "Medicaid CMS-64 FFCRA Increased FMAP Expenditure",
    cms64_adult                   = "Medicaid CMS-64 New Adult Group Expenditures",
    managed_care_summary          = "Managed Care Enrollment Summary",
    managed_care_program_state    = "^Managed Care Programs\\sby\\sState$",
    managed_care_program_plan     = "Managed Care Enrollment by Program and Plan",
    managed_care_program_pop_all  = "Managed Care Enrollment by Program and Population \\(All\\)",
    managed_care_program_pop_dual = "Managed Care Enrollment by Program and Population \\(Duals\\)",
    managed_care_feat_pop         = "Managed Care Features By Enrollment Population",
    managed_care_feat_qa          = "Managed Care Features by QA and Performance Incentive",
    managed_care_bene_month       = "Managed Care Information for Medicaid and CHIP Beneficiaries by Month",
    managed_care_bene_year        = "Managed Care Information for Medicaid and CHIP Beneficiaries by Year",
    managed_care_share            = "Share of Medicaid Enrollees in Managed Care",
    unwind_marketplace            = "HealthCare\\.gov Marketplace Medicaid Unwinding Report",
    unwind_transition             = "HealthCare\\.gov Transitions Marketplace Medicaid Unwinding Report",
    unwind_historic               = "Separate CHIP Enrollment by Month and State\\sHistoric CAA/Unwinding Period",
    unwind_sbm                    = "State-based Marketplace \\(SBM\\) Medicaid Unwinding Report"
  ),
  temporal                        = list(
    nadac_year                    = "^NADAC$",
    managed_care_state            = "^Managed Care Programs by State$",
    caid_drug_rebate_week         = "^Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program$",
    blood_disorder                = "^Pricing Comparison for Blood Disorder Treatments$",
    state_drug_util               = "^State Drug Utilization Data$",
    healthcare_quality            = "^Child and Adult Health Care Quality Measures$"
  ),
  group = list(
    caid_demographics = list(
      group = "Medicaid and CHIP Enrollee Demographics",
      alias = c(
        "demo_wellvisit",
        "demo_mental",
        "demo_disability",
        "demo_prematurity",
        "demo_language",
        "demo_ethnicity",
        "demo_rural",
        "demo_waiver"
      )
    ),
    caid_services = list(
      group = "Services Provided to the Medicaid and CHIP Population",
      alias = c(
        "service_acute",
        "service_behavior",
        "service_perinatal",
        "service_screening",
        "service_contraceptive",
        "service_dental",
        "service_pregnancy",
        "service_telehealth",
        "service_vaccination",
        "service_bloodlead",
        "service_respiratory"
      )
    ),
    caid_benes = list(
      group = "Beneficiaries Receiving A Service",
      alias = c(
        "benes_behavior",
        "benes_physical",
        "benes_mental",
        "benes_integrated",
        "benes_nas",
        "benes_smm",
        "benes_pregnant",
        "benes_delivery"
      )
    ),
    caid_finance = list(
      group = "Medicaid Financial Management Data",
      alias = c(
        "caid_finance_mgmt",
        "caid_finance_nation"
      )
    ),
    caid_nadac_group = list(
      group = "NADAC (National Average Drug Acquisition Cost)",
      alias = c(
        "caid_nadac",
        "caid_nadac_first",
        "caid_nadac_compare"
      )
    ),
    caid_benefit_pkg = list(
      group = "Benefit Package for Medicaid and CHIP Beneficiaries",
      alias = c(
        "benefit_pkg_month",
        "benefit_pkg_year"
      )
    ),
    caid_drug = list(
      group = "Medicaid Drug Datasets",
      alias = c(
        "drug_amp_mon",
        "drug_amp_qtr",
        "drug_products",
        "drug_clot",
        "drug_pediatric",
        "drug_contact_manu",
        "drug_contact_state"
      )
    ),
    caid_dual_status = list(
      group = "Dual Status Information for Medicaid and CHIP Beneficiaries",
      alias = c(
        "dual_status_month",
        "dual_status_year"
      )
    ),
    caid_meg = list(
      group = "Major Eligibility Group Information for Medicaid and CHIP Beneficiaries",
      alias = c(
        "meg_month",
        "meg_year"
      )
    ),
    caid_cms64 = list(
      group = "Medicaid CMS-64",
      alias = c(
        "cms64_caa",
        "cms64_ffcra",
        "cms64_adult"
      )
    ),
    caid_managed_care = list(
      group = "Medicaid Managed Care Enrollment",
      alias = c(
        "managed_care_summary",
        "managed_care_program_state",
        "managed_care_program_plan",
        "managed_care_program_pop_all",
        "managed_care_program_pop_dual",
        "managed_care_feat_pop",
        "managed_care_feat_qa",
        "managed_care_bene_month",
        "managed_care_bene_year"
      )
    ),
    caid_unwind = list(
      group = "Medicaid Unwinding Report",
      alias = c(
        "unwind_marketplace",
        "unwind_transition",
        "unwind_historic",
        "unwind_sbm"
      )
    )
  )
)

#' @autoglobal
#' @noRd
open_list <- list(
  endpoint = list(
    profile_covered               = "^Covered Recipient Profile Supplement$",
    profile_physician             = "^Physician \\(Distinct\\) Profile Information$",
    profile_information           = "^Profile Information$",
    profile_mapping               = "^Provider Profile ID Mapping Table$",
    profile_entity                = "^Reporting Entity Profile Information$",
    profile_teaching              = "^Teaching Hospital Profile Information$",
    summary_dashboard             = "^Summary Dashboard",
    summary_state                 = "^State Level Payment Total and Averages for all Years$",
    summary_nature                = "^State Payment Totals and Averages Grouped by Nature of Payment for all Years$",
    summary_national              = "^National Level Payment Total and Averages for all Years$",
    summary_specialty             = "^National Level Payment Total and Averages by Provider Specialty for all Years$"
  ),
  temporal                        = list(
    payment_general               = "^General Payment Data$",
    payment_ownership             = "^Ownership Payment Data$",
    payment_research              = "^Research Payment Data$",
    grouped_covered_nature        = "^Payments Grouped by Covered Recipient and Nature of Payments$",
    grouped_covered_entity        = "^Payments Grouped by Covered Recipient and Reporting Entities$",
    grouped_entity_nature         = "^Payments Grouped by Reporting Entities and Nature of Payments$",
    grouped_entity_covered_nature = "^Payments Grouped by Reporting Entities, Covered Recipient, and Nature of Payments$",
    grouped_state_nature          = "^State Payment Totals Grouped by Nature of Payment for all Years$"
  ),
  group = list(
    profile                       = list(group = "Open Payments Profiles", alias = c("profile_covered", "profile_physician", "profile_information", "profile_mapping", "profile_entity", "profile_teaching")),
    summary                       = list(group = "Open Payments Summaries", alias = c("summary_dashboard", "summary_state", "summary_nature", "summary_national", "summary_specialty"))),
  troup = list(
    payment_grouped               = list(group = "Open Payments by Year (Grouped)", alias = c("grouped_covered_nature", "grouped_covered_entity", "grouped_entity_nature", "grouped_entity_covered_nature", "grouped_state_nature")),
    payment_detailed              = list(group = "Open Payments by Year (Detailed)", alias = c("payment_general", "payment_ownership", "payment_research")))
)

#' @autoglobal
#' @noRd
pro_list <- list(
  endpoint = list(
    asc_facility              = "^Ambulatory Surgical Center Quality Measures [-] Facility",
    asc_national              = "^Ambulatory Surgical Center Quality Measures [-] National",
    asc_state                 = "^Ambulatory Surgical Center Quality Measures [-] State",
    cahps_hospice_nation      = "^Hospice care [-] National CAHPS Hospice Survey Data$",
    cahps_hospice_provider    = "^Hospice care [-] Provider CAHPS Hospice Survey Data$",
    cahps_hospice_state       = "^Hospice care [-] State CAHPS Hospice Survey Data$",
    cahps_hhc_patient         = "^Home Health Care [-] Patient Survey \\(HHCAHPS\\) 2023Q4 to 2024Q3$",
    cahps_hhc_measure         = "^Home Health Care [-] Patient Survey \\(HHCAHPS\\) Measure Dates 2023Q4 to 2024Q3$",
    cahps_hhc_national        = "^Home Health Care [-] Patient Survey \\(HHCAHPS\\) National Data 2023Q4 to 2024Q3$",
    cahps_hhc_state           = "^Home Health Care [-] Patient Survey \\(HHCAHPS\\) State Data 2023Q4 to 2024Q3$",
    cahps_ich_esrd            = "^ESRD QIP [-] In[-]Center Hemodialysis Consumer Assessment Of Healthcare Providers And Services Systems \\(ICH CAHPS\\) Survey",
    cahps_ich_facility        = "^Patient survey \\(ICH CAHPS\\) [-] Facility$",
    cahps_ich_national        = "^Patient survey \\(ICH CAHPS\\) [-] National$",
    cahps_ich_state           = "^Patient survey \\(ICH CAHPS\\) [-] State$",
    cahps_oas_footnotes       = "^Outpatient and Ambulatory Surgery Consumer Assessment of Healthcare Providers and Systems \\(OAS CAHPS\\) survey [-] Footnotes",
    cahps_oas_asc_facility    = "^Outpatient and Ambulatory Surgery Consumer Assessment of Healthcare Providers and Systems \\(OAS CAHPS\\) survey for ambulatory surgical centers [-] Facility",
    cahps_oas_asc_national    = "^Outpatient and Ambulatory Surgery Consumer Assessment of Healthcare Providers and Systems \\(OAS CAHPS\\) survey for ambulatory surgical centers [-] National",
    cahps_oas_asc_state       = "^Outpatient and Ambulatory Surgery Consumer Assessment of Healthcare Providers and Systems \\(OAS CAHPS\\) survey for ambulatory surgical centers [-] State",
    cahps_oas_hosp_facility   = "^Outpatient and Ambulatory Surgery Consumer Assessment of Healthcare Providers and Systems \\(OAS CAHPS\\) survey for hospital outpatient departments [-] Facility",
    cahps_oas_hosp_national   = "^Outpatient and Ambulatory Surgery Consumer Assessment of Healthcare Providers and Systems \\(OAS CAHPS\\) survey for hospital outpatient departments [-] National",
    cahps_oas_hosp_state      = "^Outpatient and Ambulatory Surgery Consumer Assessment of Healthcare Providers and Systems \\(OAS CAHPS\\) survey for hospital outpatient departments [-] State",
    hcahps_pch_hospital       = "Patient Survey \\(PCH [-] HCAHPS\\) PPS[-]Exempt Cancer Hospital [-] Hospital",
    hcahps_pch_national       = "Patient Survey \\(PCH [-] HCAHPS\\) PPS[-]Exempt Cancer Hospital [-] National",
    hcahps_pch_state          = "Patient Survey \\(PCH [-] HCAHPS\\) PPS[-]Exempt Cancer Hospital [-] State",
    hcahps_hospital           = "Patient survey \\(HCAHPS\\) [-] Hospital",
    hcahps_national           = "Patient survey \\(HCAHPS\\) [-] National",
    hcahps_state              = "Patient survey \\(HCAHPS\\) [-] State",
    complication_hospital     = "^Complications and Deaths [-] Hospital",
    complication_national     = "^Complications and Deaths [-] National",
    complication_state        = "^Complications and Deaths [-] State",
    complication_pch_hospital = "^Complications and Unplanned Hospital Visits [-] PPS-Exempt Cancer Hospital [-] Hospital",
    complication_pch_national = "^Complications and Unplanned Hospital Visits [-] PPS-Exempt Cancer Hospital [-] National",
    dialysis_facility         = "^Dialysis Facility [-] Listing by Facility$",
    dialysis_national         = "^Dialysis Facility [-] National Averages$",
    dialysis_state            = "^Dialysis Facility [-] State Averages$",
    esrd_depression           = "^ESRD QIP [-] Clinical Depression Screening and Follow-up",
    esrd_complete             = "^ESRD QIP [-] Complete QIP Data",
    esrd_adequacy             = "^ESRD QIP [-] Dialysis Adequacy",
    esrd_footnotes            = "^ESRD QIP [-] Footnotes",
    esrd_hypercalcemia        = "^ESRD QIP [-] Hypercalcemia",
    esrd_medication           = "^ESRD QIP [-] Medication Reconciliation",
    esrd_infection            = "^ESRD QIP [-] NHSN Bloodstream Infection",
    esrd_event                = "^ESRD QIP [-] NHSN Dialysis Event Measure",
    esrd_waitlist             = "^ESRD QIP [-] Percentage of Prevalent Patients Waitlisted",
    esrd_hospitalization      = "^ESRD QIP [-] Standardized Hospitalization Ratio",
    esrd_readmission          = "^ESRD QIP [-] Standardized Readmission Ratio",
    esrd_transfusion          = "^ESRD QIP [-] Standardized Transfusion Ratio",
    esrd_performance          = "^ESRD QIP [-] Total Performance Scores",
    esrd_ultrafiltration      = "^ESRD QIP [-] Ultrafiltration Rate",
    esrd_vascular             = "^ESRD QIP [-] Vascular Access Topic",
    hai_hospital              = "^Healthcare Associated Infections [-] Hospital",
    hai_national              = "^Healthcare Associated Infections [-] National",
    hai_state                 = "^Healthcare Associated Infections [-] State",
    hai_pch                   = "^Safety and Healthcare[-]Associated Infection Measures [-] PPS[-]Exempt Cancer Hospital",
    he_hospital               = "^Health Equity [-] Hospital",
    he_national               = "^Health Equity [-] National",
    he_state                  = "^Health Equity [-] State",
    hhc_range                 = "^Home Health Care [-] Measure Date Range$",
    hhc_national              = "^Home Health Care [-] National Data$",
    hhc_state                 = "^Home Health Care [-] State by State Data$",
    hhc_zip                   = "^Home Health Care [-] Zip Codes$",
    hhc_agency                = "^Home Health Care Agencies$",
    hhvbp_agency              = "^Expanded Home Health Value-Based Purchasing \\(HHVBP\\) Model [-] Agency Data$",
    hhvbp_cohort              = "^Expanded Home Health Value-Based Purchasing \\(HHVBP\\) Model [-] Cohort Data$",
    hvbp_outcomes             = "^Hospital Value[-]Based Purchasing \\(HVBP\\) [-] Clinical Outcomes Domain Scores",
    hvbp_efficiency           = "^Hospital Value[-]Based Purchasing \\(HVBP\\) [-] Efficiency Scores",
    hvbp_engagement           = "^Hospital Value[-]Based Purchasing \\(HVBP\\) [-] Person and Community Engagement Domain Scores \\(HCAHPS\\)",
    hvbp_safety               = "^Hospital Value[-]Based Purchasing \\(HVBP\\) [-] Safety",
    hvbp_performance          = "^Hospital Value[-]Based Purchasing \\(HVBP\\) [-] Total Performance Score",
    hospice_general           = "^Hospice [-] General Information$",
    hospice_provider          = "^Hospice [-] Provider Data$",
    hospice_state             = "^Hospice [-] State Data$",
    hospice_zip               = "^Hospice [-] Zip Data$",
    hospice_national          = "^Hospice[-]National Data$",
    hospital_psi90            = "^CMS Medicare PSI[-]90 and component measures [-] six[-]digit estimate dataset",
    hospital_joint            = "^Comprehensive Care For Joint Replacement Model [-] Provider Data",
    hospital_footnote         = "^Footnote Crosswalk",
    hospital_update           = "^Data Updates",
    hospital_dates            = "^Measure Dates",
    hospital_maternal         = "^Maternal Health [-] Hospital",
    hospital_outcomes         = "^Patient[-]Reported Outcomes [-] Hospital",
    hospital_general          = "^Hospital General Information",
    hospital_pi               = "^Promoting Interoperability [-] Hospital",
    hospital_voc_nation       = "^Value of care [-] National",
    hospital_voc_hosp         = "^Payment and value of care [-] Hospital",
    hospital_pmt_state        = "^Payment [-] State",
    hospital_pmt_nation       = "^Payment [-] National",
    hospital_drg_net          = "^Table 1[:] FY2021 Net Change in Base Operating DRG Payment Amount",
    hospital_drg_dist         = "^Table 2[:] FY2021 Distribution of Net Change in Base Operating DRG Payment Amount",
    hospital_pmt_pct          = "^Table 3[:] FY2021 Percent Change in Medicare Payments",
    hospital_pmt_vbi          = "^Table 4[:] FY2021 Value-Based Incentive Payment Amount",
    ipf_national              = "^Inpatient Psychiatric Facility Quality Measure Data [-] National",
    ipf_facility              = "^Inpatient Psychiatric Facility Quality Measure Data [-] by Facility",
    ipf_state                 = "^Inpatient Psychiatric Facility Quality Measure Data [-] by State",
    irf_conditions            = "^Inpatient Rehabilitation Facility [-] Conditions$",
    irf_general               = "^Inpatient Rehabilitation Facility [-] General Information$",
    irf_provider              = "^Inpatient Rehabilitation Facility [-] Provider Data$",
    irf_national              = "^Inpatient Rehabilitation Facility [-] National Data$",
    ltch_general              = "^Long-Term Care Hospital [-] General Information$",
    ltch_provider             = "^Long-Term Care Hospital [-] Provider Data$",
    ltch_national             = "^Long-Term Care Hospital [-] National Data$",
    mips_performance          = "^PY 2022 Clinician Public Reporting[:] Overall MIPS Performance$",
    mips_patient              = "^PY 2022 Group Public Reporting[:] Patient Experience$",
    mips_clinician            = "^PY 2022 Clinician Public Reporting[:] MIPS Measures and Attestations$",
    mips_group                = "^PY 2022 Group Public Reporting[:] MIPS Measures and Attestations$",
    mips_virtual              = "^PY 2022 Virtual Group Public Reporting[:] MIPS Measures and Attestations$",
    mspb_claim                = "^Medicare Hospital Spending by Claim",
    mspb_hospital             = "^Medicare Spending Per Beneficiary [-] Hospital$",
    mspb_decimal              = "^Medicare Spending Per Beneficiary [-] Hospital Additional Decimal Places$",
    mspb_national             = "^Medicare Spending Per Beneficiary [-] National$",
    mspb_state                = "^Medicare Spending Per Beneficiary [-] State$",
    nursing_ownership         = "^Ownership$",
    nursing_penalties         = "^Penalties$",
    nursing_provider          = "^Provider Information$",
    nursing_citation          = "^Citation Code Look[-]up$",
    nursing_fire              = "^Fire Safety Deficiencies$",
    nursing_deficiencies      = "^Health Deficiencies$",
    nursing_inspection        = "^Inspection Dates$",
    nursing_quality_mds       = "^MDS Quality Measures$",
    nursing_quality_claims    = "^Medicare Claims Quality Measures$",
    nursing_state_avg         = "^State US Averages$",
    nursing_state_cut         = "^State-Level Health Inspection Cut Points$",
    nursing_interval          = "^Nursing Home Data Collection Interval",
    out_img_hospital          = "^Outpatient Imaging Efficiency [-] Hospital",
    out_img_national          = "^Outpatient Imaging Efficiency [-] National",
    out_img_state             = "^Outpatient Imaging Efficiency [-] State",
    pch_pall_hospital         = "^Palliative Care [-] PPS-Exempt Cancer Hospital [-] Hospital",
    pch_pall_national         = "^Palliative Care [-] PPS-Exempt Cancer Hospital [-] National",
    pdc_affiliations          = "^Facility Affiliation Data$",
    pdc_clinicians            = "^National Downloadable File$",
    pdc_utilization           = "^Utilization Data$",
    reduction_hac             = "Hospital[-]Acquired Condition \\(HAC\\) Reduction Program",
    reduction_hrr             = "Hospital Readmissions Reduction Program",
    snf_vbp_performance       = "^FY 2025 SNF VBP Aggregate Performance$",
    snf_vbp_facility          = "^FY 2025 SNF VBP Facility[-]Level Dataset$",
    snf_quality_nation        = "^Skilled Nursing Facility Quality Reporting Program [-] National Data$",
    snf_quality_provider      = "^Skilled Nursing Facility Quality Reporting Program [-] Provider Data$",
    snf_quality_swing         = "^Skilled Nursing Facility Quality Reporting Program [-] Swing Beds [-] Provider Data$",
    supplier_directory        = "^Medical Equipment Suppliers$",
    timely_hospital           = "^Timely and Effective Care [-] Hospital",
    timely_national           = "^Timely and Effective Care [-] National",
    timely_state              = "^Timely and Effective Care [-] State",
    unplan_hospital           = "^Unplanned Hospital Visits [-] Hospital",
    unplan_national           = "^Unplanned Hospital Visits [-] National",
    unplan_state              = "^Unplanned Hospital Visits [-] State",
    va_behavioral             = "^Veterans Health Administration Behavioral Health Data",
    va_provider               = "^Veterans Health Administration Provider Level Data",
    va_timely                 = "^Veterans Health Administration Timely and Effective Care Data"
  ),
  group = list(
    pro_cahps_spice           = list(group = "CAHPS Hospice Survey Data", alias = c("cahps_hospice_nation", "cahps_hospice_provider", "cahps_hospice_state")),
    pro_cahps_hhc             = list(group = "Home Health Care Patient Survey Data (HHCAHPS)", alias = c("cahps_hhc_patient", "cahps_hhc_measure", "cahps_hhc_national", "cahps_hhc_state")),
    pro_cahps_ich             = list(group = "In-Center Hemodialysis Consumer Assessment Of Healthcare Providers And Services Systems (ICH CAHPS) Survey", alias = c("cahps_ich_esrd", "cahps_ich_facility", "cahps_ich_national", "cahps_ich_state")),
    pro_cahps_oas             = list(group = "Outpatient and Ambulatory Surgery Consumer Assessment of Healthcare Providers and Systems (OAS CAHPS) Survey", alias = c("cahps_oas_footnotes", "cahps_oas_asc_facility", "cahps_oas_asc_national", "cahps_oas_asc_state", "cahps_oas_hosp_facility", "cahps_oas_hosp_national", "cahps_oas_hosp_state")),
    pro_mips                  = list(group = "PY 2022 MIPS Public Reporting", alias = c("mips_performance", "mips_patient", "mips_clinician", "mips_group", "mips_virtual")),
    pro_drs                   = list(group = "Provider Data Catalog", alias = c("pdc_affiliations", "pdc_clinicians", "pdc_utilization")),
    pro_ltch                  = list(group = "Long-Term Care Hospitals", alias = c("ltch_general", "ltch_provider", "ltch_national")),
    pro_irf                   = list(group = "Inpatient Rehabilitation Facilities", alias = c("irf_conditions", "irf_general", "irf_provider", "irf_national")),
    pro_hospice               = list(group = "Hospices", alias = c("hospice_general", "hospice_provider", "hospice_state", "hospice_zip", "hospice_national")),
    pro_hhc_vbp               = list(group = "Expanded Home Health Value-Based Purchasing (HHVBP) Model", alias = c("hhvbp_agency", "hhvbp_cohort")),
    pro_home_health           = list(group = "Home Health Care Agencies", alias = c("hhc_range", "hhc_national", "hhc_state", "hhc_zip", "hhc_agency")),
    pro_snf_vbp               = list(group = "FY 2025 SNF VBP", alias = c("snf_vbp_performance", "snf_vbp_facility")),
    pro_snf_quality           = list(group = "SNF Quality Measures", alias = c("snf_quality_nation", "snf_quality_provider", "snf_quality_swing")),
    pro_nursing               = list(group = "Nursing Homes", alias = c("nursing_ownership", "nursing_penalties", "nursing_provider", "nursing_citation", "nursing_fire", "nursing_deficiencies", "nursing_inspection", "nursing_quality_mds", "nursing_quality_claims", "nursing_state_avg", "nursing_state_cut", "nursing_interval")),
    pro_complication          = list(group = "Complications and Deaths", alias = c("complication_hospital", "complication_state", "complication_national")),
    pro_complication_pch      = list(group = "Complications and Unplanned Hospital Visits: PPS-Exempt Cancer Hospital", alias = c("complication_pch_hospital", "complication_pch_national")),
    pro_asc_quality           = list(group = "Ambulatory Surgical Center Quality Measures", alias = c("asc_facility", "asc_national", "asc_state")),
    pro_equity                = list(group = "Health Equity", alias = c("he_hospital", "he_national", "he_state")),
    pro_hai                   = list(group = "Healthcare Associated Infections", alias = c("hai_hospital", "hai_national", "hai_state", "hai_PCH")),
    pro_dialysis              = list(group = "Dialysis Facilities", alias = c("dialysis_facility", "dialysis_national", "dialysis_state")),
    pro_esrd                  = list(group = "ESRD QIP", alias = c("esrd_depression", "esrd_complete", "esrd_adequacy", "esrd_footnotes", "esrd_hypercalcemia", "esrd_medication", "esrd_infection", "esrd_event", "esrd_waitlist", "esrd_hospitalization", "esrd_readmission", "esrd_transfusion", "esrd_performance", "esrd_ultrafiltration", "esrd_vascular")),
    pro_hvbp                  = list(group = "Hospital Value-Based Purchasing (HVBP)", alias = c("hvbp_outcomes", "hvbp_efficiency", "hvbp_engagement", "hvbp_safety", "hvbp_performance")),
    pro_ipf                   = list(group = "Inpatient Psychiatric Facility Quality Measure Data", alias = c("ipf_national", "ipf_facility", "ipf_state")),
    pro_mspb                  = list(group = "Medicare Spending Per Beneficiary", alias = c("mspb_claim", "mspb_hospital", "mspb_decimal", "mspb_national", "mspb_state")),
    pro_out_img               = list(group = "Outpatient Imaging Efficiency", alias = c("out_img_hospital", "out_img_national", "out_img_state")),
    pro_pch_pall              = list(group = "Palliative Care: PPS-Exempt Cancer Hospital", alias = c("pch_pall_hospital", "pch_pall_national")),
    pro_pch_hcahps            = list(group = "Patient Survey (PCH HCAHPS) PPS-Exempt Cancer Hospital", alias = c("hcahps_pch_hospital", "hcahps_pch_national", "hcahps_pch_state")),
    pro_hcahps                = list(group = "Patient Survey (HCAHPS)", alias = c("hcahps_hospital", "hcahps_national", "hcahps_state")),
    pro_timely                = list(group = "Timely and Effective Care", alias = c("timely_hospital", "timely_national", "timely_state")),
    pro_unplan                = list(group = "Unplanned Hospital Visits", alias = c("unplan_hospital", "unplan_national", "unplan_state")),
    pro_va                    = list(group = "Veterans Health Administration", alias = c("va_behavioral", "va_provider", "va_timely")),
    pro_hospital_changes      = list(group = "Hospital FY2021 Changes in Payment", alias = c("hospital_drg_net", "hospital_drg_dist", "hospital_pmt_pct", "hospital_pmt_vbi")),
    pro_hospital_voc          = list(group = "Payment and Value of Care", alias = c("hospital_voc_nation", "hospital_voc_hosp", "hospital_pmt_state", "hospital_pmt_nation")),
    pro_reduction             = list(group = "Hospital-Acquired Condition & Readmission Reduction Programs", alias = c("reduction_hac", "reduction_hrr"))
  )
)

#' @autoglobal
#' @noRd
make_quick_entry <- function(x) {
  x |> names() |> cat(sep = " = ,\n")
}

# #' @autoglobal
# #' @noRd
# example_list <- list(
#   endpoint = list(
#     ALIAS = "NAME"
#   ),
#   temporal = list(
#     ALIAS = "NAME"
#   )
# )

# temp1 <- '{names(caid_temp_list)} = "{unlist(caid_temp_list, use.names = FALSE)}", '
# temp2 <- 'switch(x, <<xx>> cli_abort(c("x" = "No matches found for {.val {x}}."), call = call))'
#
# xx <- glue(temp1) |> glue_collapse(sep = "\n")
# x  <- glue(temp2, .open = "<<", .close = ">>") |> parse_expr() |> eval_bare()
