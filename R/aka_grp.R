# ---- caid ----
#' @autoglobal
#' @noRd
grp_caid = list(
  caid_demographics = list(
    name  = "Medicaid and CHIP Enrollee Demographics",
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
    name = "Services Provided to the Medicaid and CHIP Population",
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
    name = "Beneficiaries Receiving A Service",
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
    name = "Medicaid Financial Management Data",
    alias = c(
      "caid_finance_mgmt",
      "caid_finance_nation"
    )
  ),
  caid_nadac_group = list(
    name = "NADAC (National Average Drug Acquisition Cost)",
    alias = c(
      "caid_nadac",
      "caid_nadac_first",
      "caid_nadac_compare"
    )
  ),
  caid_pkg = list(
    name = "Benefit Package for Medicaid and CHIP Beneficiaries",
    alias = c(
      "benefit_pkg_month",
      "benefit_pkg_year"
    )
  ),
  caid_drug = list(
    name = "Medicaid Drug Datasets",
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
  caid_dual = list(
    name = "Dual Status Information for Medicaid and CHIP Beneficiaries",
    alias = c(
      "dual_status_month",
      "dual_status_year"
    )
  ),
  caid_meg = list(
    name = "Major Eligibility Group Information for Medicaid and CHIP Beneficiaries",
    alias = c(
      "meg_month",
      "meg_year"
    )
  ),
  caid_cms64 = list(
    name = "Medicaid CMS-64",
    alias = c(
      "cms64_caa",
      "cms64_ffcra",
      "cms64_adult"
    )
  ),
  caid_managed = list(
    name = "Medicaid Managed Care Enrollment",
    alias = c(
      "managed_summary",
      "managed_state",
      "managed_program",
      "managed_pop",
      "managed_dual",
      "managed_feat_pop",
      "managed_feat_qa",
      "managed_bene_month",
      "managed_bene_year"
    )
  ),
  caid_unwind = list(
    name = "Medicaid Unwinding Report",
    alias = c(
      "unwind_market",
      "unwind_transition",
      "unwind_historic",
      "unwind_sbm"
    )
  )
)

# ---- open ----
#' @autoglobal
#' @noRd
grp_open = list(
  profile = list(
    name = "Open Payments Profiles",
    alias = c(
      "profile_covered",
      "profile_physician",
      "profile_information",
      "profile_mapping",
      "profile_entity",
      "profile_teaching"
    )
  ),
  summary = list(
    name = "Open Payments Summaries",
    alias = c(
      "summary_dashboard",
      "summary_state",
      "summary_nature",
      "summary_national",
      "summary_specialty"
    )
  ),
  payment_grouped = list(
    name = "Open Payments by Year (Grouped)",
    alias = c(
      "grouped_covered_nature",
      "grouped_covered_entity",
      "grouped_entity_nature",
      "grouped_entity_covered_nature",
      "grouped_state_nature"
    )
  ),
  payment_detailed = list(
    name = "Open Payments by Year (Detailed)",
    alias = c("payment_general", "payment_ownership", "payment_research")
  )
)

# ---- prov ----
#' @autoglobal
#' @noRd
grp_prov = list(
  prov_cahps_spice = list(
    name = "CAHPS Hospice Survey Data",
    alias = c(
      "cahps_hospice_nation",
      "cahps_hospice_provider",
      "cahps_hospice_state"
    )
  ),
  prov_cahps_hhc = list(
    name = "Home Health Care Patient Survey Data (HHCAHPS)",
    alias = c(
      "cahps_hhc_patient",
      "cahps_hhc_measure",
      "cahps_hhc_national",
      "cahps_hhc_state"
    )
  ),
  prov_cahps_ich = list(
    name = "In-Center Hemodialysis Consumer Assessment Of Healthcare Providers And Services Systems (ICH CAHPS) Survey",
    alias = c(
      "cahps_ich_esrd",
      "cahps_ich_facility",
      "cahps_ich_national",
      "cahps_ich_state"
    )
  ),
  prov_cahps_oas = list(
    name = "Outpatient and Ambulatory Surgery Consumer Assessment of Healthcare Providers and Systems (OAS CAHPS) Survey",
    alias = c(
      "cahps_oas_footnotes",
      "cahps_oas_asc_facility",
      "cahps_oas_asc_national",
      "cahps_oas_asc_state",
      "cahps_oas_hosp_facility",
      "cahps_oas_hosp_national",
      "cahps_oas_hosp_state"
    )
  ),
  prov_mips = list(
    name = "PY 2022 MIPS Public Reporting",
    alias = c(
      "mips_performance",
      "mips_patient",
      "mips_clinician",
      "mips_group",
      "mips_virtual"
    )
  ),
  prov_drs = list(
    name = "Provider Data Catalog",
    alias = c("pdc_affiliations", "pdc_clinicians", "pdc_utilization")
  ),
  prov_ltch = list(
    name = "Long-Term Care Hospitals",
    alias = c("ltch_general", "ltch_provider", "ltch_national")
  ),
  prov_irf = list(
    name = "Inpatient Rehabilitation Facilities",
    alias = c(
      "irf_conditions",
      "irf_general",
      "irf_provider",
      "irf_national"
    )
  ),
  prov_hospice = list(
    name = "Hospices",
    alias = c(
      "hospice_general",
      "hospice_provider",
      "hospice_state",
      "hospice_zip",
      "hospice_national"
    )
  ),
  prov_hhc_vbp = list(
    name = "Expanded Home Health Value-Based Purchasing (HHVBP) Model",
    alias = c("hhvbp_agency", "hhvbp_cohort")
  ),
  prov_home_health = list(
    name = "Home Health Care Agencies",
    alias = c(
      "hhc_range",
      "hhc_national",
      "hhc_state",
      "hhc_zip",
      "hhc_agency"
    )
  ),
  prov_snf_vbp = list(
    name = "FY 2025 SNF VBP",
    alias = c("snf_vbp_performance", "snf_vbp_facility")
  ),
  prov_snf_quality = list(
    name = "SNF Quality Measures",
    alias = c(
      "snf_quality_nation",
      "snf_quality_provider",
      "snf_quality_swing"
    )
  ),
  prov_nursing = list(
    name = "Nursing Homes",
    alias = c(
      "nursing_ownership",
      "nursing_penalties",
      "nursing_provider",
      "nursing_citation",
      "nursing_fire",
      "nursing_deficiencies",
      "nursing_inspection",
      "nursing_quality_mds",
      "nursing_quality_claims",
      "nursing_state_avg",
      "nursing_state_cut",
      "nursing_interval"
    )
  ),
  prov_complication = list(
    name = "Complications and Deaths",
    alias = c(
      "complication_hospital",
      "complication_state",
      "complication_national"
    )
  ),
  prov_complication_pch = list(
    name = "Complications and Unplanned Hospital Visits: PPS-Exempt Cancer Hospital",
    alias = c("complication_pch_hospital", "complication_pch_national")
  ),
  prov_asc_quality = list(
    name = "Ambulatory Surgical Center Quality Measures",
    alias = c("asc_facility", "asc_national", "asc_state")
  ),
  prov_equity = list(
    name = "Health Equity",
    alias = c("he_hospital", "he_national", "he_state")
  ),
  prov_hai = list(
    name = "Healthcare Associated Infections",
    alias = c("hai_hospital", "hai_national", "hai_state", "hai_PCH")
  ),
  prov_dialysis = list(
    name = "Dialysis Facilities",
    alias = c(
      "dialysis_by_facility",
      "dialysis_national",
      "dialysis_state"
    )
  ),
  prov_esrd = list(
    name = "ESRD QIP",
    alias = c(
      "esrd_depression",
      "esrd_complete",
      "esrd_adequacy",
      "esrd_footnotes",
      "esrd_hypercalcemia",
      "esrd_medication",
      "esrd_infection",
      "esrd_event",
      "esrd_waitlist",
      "esrd_hospitalization",
      "esrd_readmission",
      "esrd_transfusion",
      "esrd_performance",
      "esrd_ultrafiltration",
      "esrd_vascular"
    )
  ),
  prov_hvbp = list(
    name = "Hospital Value-Based Purchasing (HVBP)",
    alias = c(
      "hvbp_outcomes",
      "hvbp_efficiency",
      "hvbp_engagement",
      "hvbp_safety",
      "hvbp_performance"
    )
  ),
  prov_ipf = list(
    name = "Inpatient Psychiatric Facility Quality Measure Data",
    alias = c("ipf_national", "ipf_facility", "ipf_state")
  ),
  prov_mspb = list(
    name = "Medicare Spending Per Beneficiary",
    alias = c(
      "mspb_claim",
      "mspb_hospital",
      "mspb_decimal",
      "mspb_national",
      "mspb_state"
    )
  ),
  prov_out_img = list(
    name = "Outpatient Imaging Efficiency",
    alias = c("out_img_hospital", "out_img_national", "out_img_state")
  ),
  prov_pch_pall = list(
    name = "Palliative Care: PPS-Exempt Cancer Hospital",
    alias = c("pch_pall_hospital", "pch_pall_national")
  ),
  prov_pch_hcahps = list(
    name = "Patient Survey (PCH HCAHPS) PPS-Exempt Cancer Hospital",
    alias = c(
      "hcahps_pch_hospital",
      "hcahps_pch_national",
      "hcahps_pch_state"
    )
  ),
  prov_hcahps = list(
    name = "Patient Survey (HCAHPS)",
    alias = c("hcahps_hospital", "hcahps_national", "hcahps_state")
  ),
  pro_timely = list(
    name = "Timely and Effective Care",
    alias = c("timely_hospital", "timely_national", "timely_state")
  ),
  prov_unplan = list(
    name = "Unplanned Hospital Visits",
    alias = c("unplan_hospital", "unplan_national", "unplan_state")
  ),
  prov_vha = list(
    name = "Veterans Health Administration",
    alias = c("va_behavioral", "va_provider", "va_timely")
  ),
  prov_hospital_changes = list(
    name = "Hospital FY2021 Changes in Payment",
    alias = c(
      "hospital_drg_net",
      "hospital_drg_dist",
      "hospital_pmt_pct",
      "hospital_pmt_vbi"
    )
  ),
  prov_hospital_voc = list(
    name = "Payment and Value of Care",
    alias = c(
      "hospital_voc_nation",
      "hospital_voc_hosp",
      "hospital_pmt_state",
      "hospital_pmt_nation"
    )
  ),
  prov_reduction = list(
    name = "Hospital-Acquired Condition & Readmission Reduction Programs",
    alias = c("reduction_hac", "reduction_hrr")
  )
)

# ---- prov ----
#' @autoglobal
#' @noRd
grp_care = list(
  care_hha = list(
    name = "Home Health Agencies",
    alias = c("hha_owners", "hha_costreport", "hha_enrollments")
  ),
  care_hospice = list(
    name = "Hospices",
    alias = c("hospice_owners", "hospice_enrollments", "hospice_acute")
  ),
  care_hospital = list(
    name = "Hospitals",
    alias = c(
      "hospital_owners",
      "hospital_chow",
      "hospital_chow_owner",
      "hospital_enrollments",
      "hospital_costreport",
      "hospital_service_area"
    )
  ),
  care_rhc = list(
    name = "Rural Health Clinics",
    alias = c("rhc_owners", "rhc_enrollments")
  ),
  care_fqhc = list(
    name = "Federally Qualified Health Centers",
    alias = c("fqhc_owners", "fqhc_enrollments")
  ),
  care_pend = list(
    name = "Pending Initial Logging and Tracking",
    alias = c("pilat_non_physician", "pilat_physician")
  ),
  care_reval = list(
    name = "Revalidation Reassignment Lists",
    alias = c("revalid_group", "revalid_due", "revalid_list")
  ),
  care_snf = list(
    name = "Skilled Nursing Facilities",
    alias = c(
      "snf_owners",
      "snf_chow",
      "snf_chow_owner",
      "snf_cost_report",
      "snf_enrollments"
    )
  ),
  care_aco = list(
    name = "Accountable Care Organizations",
    alias = c(
      "aco_reach_aligned",
      "aco_reach_eligible",
      "aco_reach_results",
      "aco_reach_providers",
      "aco_reach_orgs",
      "aco_pioneer",
      "aco_participants",
      "aco_snf_affiliate",
      "aco_organizations",
      "aco_bene_cnty"
    )
  ),
  care_stats = list(
    name = "CMS Program Statistics",
    alias = c(
      "cms_ma_enroll",
      "cms_ma_outpatient",
      "cms_ma_other",
      "cms_ma_inpatient",
      "cms_ma_snf",
      "cms_deaths",
      "cms_hha",
      "cms_hospice",
      "cms_inpatient",
      "cms_new_enroll",
      "cms_outpatient",
      "cms_tos",
      "cms_partd",
      "cms_partd_enroll",
      "cms_phys_npp_supp",
      "cms_premiums",
      "cms_providers",
      "cms_snf",
      "cms_total_enroll",
      "cms_dual_enroll",
      "cms_orig_enroll"
    )
  ),
  care_caid = list(
    name = "Medicaid",
    alias = c(
      "care_caid_managed_care",
      "care_caid_opioid_geo",
      "care_caid_drug_spend"
    )
  ),
  care_geo = list(
    name = "Medicare Geographic Variation",
    alias = c("geovar_adv", "geovar_hrr", "geovar_nsc")
  ),
  care_pdp = list(
    name = "Pharmacy Network/Formulary/Pricing",
    alias = c("pdp_month", "pdp_quarter")
  ),
  care_survey = list(
    name = "Medicare Current Beneficiary Survey",
    alias = c("bene_survey_covid", "bene_survey_cost", "bene_survey_file")
  ),
  care_drugb = list(
    name = "Medicare Part B Drugs",
    alias = c("partb_drug_discard", "partb_drug_spend")
  ),
  care_drugd = list(
    name = "Medicare Part D Drugs",
    alias = c("partd_opioid", "partd_drug_spend")
  ),
  care_market = list(
    name = "Market Saturation & Utilization",
    alias = c("market_cbsa", "market_state_cnty")
  ),
  care_in = list(
    name = "Medicare Inpatient Hospitals",
    alias = c(
      "inpatient_geography",
      "inpatient_provider",
      "inpatient_service"
    )
  ),
  care_out = list(
    name = "Medicare Outpatient Hospitals",
    alias = c("outpatient_geography", "outpatient_service")
  ),
  care_prx = list(
    name = "Medicare Part D Prescribers",
    alias = c("prx_geography", "prx_provider", "prx_drug")
  ),
  care_dme = list(
    name = "Medicare DME, Devices & Supplies",
    alias = c(
      "dme_geography",
      "dme_provider",
      "dme_service",
      "dme_supplier",
      "dme_supplier_service"
    )
  ),
  care_pbj = list(
    name = "Nursing Home Payroll-Based Journal Staffing",
    alias = c(
      "nhome_staff_nonurse",
      "nhome_staff_nurse",
      "nhome_staff_employee"
    )
  ),
  care_utilization = list(
    name = "Medicare Physician & Other Practitioners",
    alias = c("util_geography", "util_provider", "util_service")
  ),
  care_nhome = list(
    name = "Nursing Home Performance",
    alias = c("nh_performance", "nh_mds_frequency", "nh_mds_facility")
  )
)


# ---- aka_names ----
#' @autoglobal
#' @noRd
grp_names <- mph_init(
  names(c(
    grp_prov,
    grp_caid,
    grp_care,
    grp_open
  )))

# ---- aka_regex ----
#' @autoglobal
#' @noRd
grp_regex <- c(
    grp_prov,
    grp_caid,
    grp_care,
    grp_open
)

#' @autoglobal
#' @noRd
group_regex <- function(x) {
  grp_regex[mph_match(x, grp_names)]
}

#' @autoglobal
#' @noRd
is_api_group <- function(x) {
  !is.na(mph_match(x, grp_names))
}

