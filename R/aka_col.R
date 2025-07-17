#' @include aka_end.R
NULL

# ---- caid ----
#' @autoglobal
#' @noRd
col_caid = list(
  caid_demographics = list(
    name  = "Medicaid and CHIP Enrollee Demographics",
    alias = grep("^demo", names(end_caid$current), value = TRUE)),
  caid_services = list(
    name = "Services Provided to the Medicaid and CHIP Population",
    alias = grep("^service", names(end_caid$current), value = TRUE)),
  caid_benes = list(
    name = "Beneficiaries Receiving A Service",
    alias = grep("^benes", names(end_caid$current), value = TRUE)),
  caid_finance = list(
    name = "Medicaid Financial Management Data",
    alias = grep("_fin_", names(end_caid$current), value = TRUE)),
  caid_nadac_group = list(
    name = "NADAC (National Average Drug Acquisition Cost)",
    alias = grep("^nadac", names(c(end_caid$current, end_caid$temporal)), value = TRUE)),
  caid_pkg = list(
    name = "Benefit Package for Medicaid and CHIP Beneficiaries",
    alias = grep("^benefit", names(end_caid$current), value = TRUE)),
  caid_drug = list(
    name = "Medicaid Drug Datasets",
    alias = grep("^drug", names(end_caid$current), value = TRUE)),
  caid_dual = list(
    name = "Dual Status Information for Medicaid and CHIP Beneficiaries",
    alias = grep("^dual_status", names(end_caid$current), value = TRUE)),
  caid_meg = list(
    name = "Major Eligibility Group Information for Medicaid and CHIP Beneficiaries",
    alias = grep("^meg", names(end_caid$current), value = TRUE)),
  caid_cms64 = list(
    name = "Medicaid CMS-64",
    alias = grep("^cms64", names(end_caid$current), value = TRUE)),
  caid_managed = list(
    name = "Managed Care Enrollment",
    alias = grep("^managed", names(end_caid$current), value = TRUE)),
  caid_unwind = list(
    name = "Medicaid Unwinding Report",
    alias = grep("^unwind", names(end_caid$current), value = TRUE)))

# ---- open ----
#' @autoglobal
#' @noRd
col_open = list(
  profile = list(
    name = "Open Payments Profiles",
    alias = grep("^profile", names(end_open$current), value = TRUE)),
  summary = list(
    name = "Open Payments Summaries",
    alias = grep("^summary", names(end_open$current), value = TRUE)),
  payment_grouped = list(
    name = "Open Payments by Year (Grouped)",
    alias = grep("^grouped", names(end_open$temporal), value = TRUE)),
  payment_detailed = list(
    name = "Open Payments by Year (Detailed)",
    alias = grep("^payment", names(end_open$temporal), value = TRUE)))

# ---- prov ----
#' @autoglobal
#' @noRd
col_prov = list(
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
    alias = grep("^mips", names(end_prov$current), value = TRUE)),
  prov_pdc = list(
    name = "Provider Data Catalog",
    alias = grep("^pdc", names(end_prov$current), value = TRUE)),
  prov_ltch = list(
    name = "Long-Term Care Hospitals",
    alias = grep("^ltch", names(end_prov$current), value = TRUE)),
  prov_irf = list(
    name = "Inpatient Rehabilitation Facilities",
    alias = grep("^irf", names(end_prov$current), value = TRUE)),
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
    alias = grep("^hhvbp", names(end_prov$current), value = TRUE)),
  prov_home_health = list(
    name = "Home Health Care Agencies",
    alias = grep("^hhc", names(end_prov$current), value = TRUE)),
  prov_snf_vbp = list(
    name = "FY 2025 SNF VBP",
    alias = grep("^snf_vbp", names(end_prov$current), value = TRUE)),
  prov_snf_quality = list(
    name = "SNF Quality Measures",
    alias = grep("^snf_quality", names(end_prov$current), value = TRUE)),
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
    alias = grep("^complication_pch", names(end_prov$current), value = TRUE)),
  prov_asc_quality = list(
    name = "Ambulatory Surgical Center Quality Measures",
    alias = grep("^asc", names(end_prov$current), value = TRUE)),
  prov_equity = list(
    name = "Health Equity",
    alias = grep("^he_", names(end_prov$current), value = TRUE)),
  prov_hai = list(
    name = "Healthcare Associated Infections",
    alias = grep("^hai", names(end_prov$current), value = TRUE)),
  prov_dialysis = list(
    name = "Dialysis Facilities",
    alias = grep("^dialysis", names(end_prov$current), value = TRUE)),
  prov_esrd = list(
    name = "ESRD QIP",
    alias = grep("^esrd", names(end_prov$current), value = TRUE)),
  prov_hvbp = list(
    name = "Hospital Value-Based Purchasing (HVBP)",
    alias = grep("^hvbp", names(end_prov$current), value = TRUE)),
  prov_ipf = list(
    name = "Inpatient Psychiatric Facility Quality Measure Data",
    alias = grep("^ipf", names(end_prov$current), value = TRUE)),
  prov_mspb = list(
    name = "Medicare Spending Per Beneficiary",
    alias = grep("^mspb", names(end_prov$current), value = TRUE)),
  prov_out_img = list(
    name = "Outpatient Imaging Efficiency",
    alias = grep("^out_img", names(end_prov$current), value = TRUE)),
  prov_pch_pall = list(
    name = "Palliative Care: PPS-Exempt Cancer Hospital",
    alias = grep("^pch_pall", names(end_prov$current), value = TRUE)),
  prov_pch_hcahps = list(
    name = "Patient Survey (PCH HCAHPS) PPS-Exempt Cancer Hospital",
    alias = grep("^hcahps_pch", names(end_prov$current), value = TRUE)),
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
    alias = grep("^va_", names(end_prov$current), value = TRUE)
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
    alias = c(
      "reduction_hac",
      "reduction_hrr"
    )
  )
)

# ---- prov ----
#' @autoglobal
#' @noRd
col_care = list(
  care_hha = list(
    name = "Home Health Agencies",
    alias = c(
      "hha_owner",
      "hha_costrep_tmp",
      "hha_enroll"
    )
  ),
  care_hospice = list(
    name = "Hospices",
    alias = c(
      "spice_owner",
      "spice_enroll",
      "spice_acute"
    )
  ),
  care_hospital = list(
    name = "Hospitals",
    alias = c(
      "hosp_owner",
      "hosp_chow",
      "hosp_chow_owner",
      "hosp_enroll",
      "hosp_costrep_tmp",
      "hosp_service"
    )
  ),
  care_rhc = list(
    name = "Rural Health Clinics",
    alias = c(
      "rhc_owner",
      "rhc_enroll"
    )
  ),
  care_fqhc = list(
    name = "Federally Qualified Health Centers",
    alias = c(
      "fqhc_owner",
      "fqhc_enroll"
    )
  ),
  care_pend = list(
    name = "Pending Initial Logging and Tracking",
    alias = c(
      "pilat_non",
      "pilat_phys"
    )
  ),
  care_reval = list(
    name = "Revalidation Reassignments",
    alias = c(
      "reval_group",
      "reval_due",
      "reval_list"
    )
  ),
  care_snf = list(
    name = "Skilled Nursing Facilities",
    alias = c(
      "snf_owner",
      "snf_chow",
      "snf_chow_owner",
      "snf_costrep_tmp",
      "snf_enroll"
    )
  ),
  care_aco = list(
    name = "Accountable Care Organizations",
    alias = c(
      "aco_parts",
      "aco_snfs",
      "aco_orgs",
      "aco_bene_cnty",
      "aco_shared"
    )
  ),
  care_aco_reach = list(
    name = "Accountable Care Organizations",
    alias = c(
      "aco_reach_align",
      "aco_reach_elig",
      "aco_reach_result",
      "aco_reach_prov",
      "aco_reach_orgs"
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
    name = "Medicare Medicaid",
    alias = c(
      "care_caid_managed",
      "care_caid_opioid",
      "care_caid_spend"
    )
  ),
  care_geo = list(
    name = "Medicare Geographic Variation",
    alias = c(
      "geo_ma",
      "geo_hrr",
      "geo_nsc"
    )
  ),
  care_pdp = list(
    name = "Pharmacy Network/Formulary/Pricing",
    alias = c(
      "pdp_month",
      "pdp_quarter"
    )
  ),
  care_survey = list(
    name = "Medicare Current Beneficiary Survey",
    alias = c(
      "care_survey_covid",
      "care_survey_cost",
      "care_survey"
    )
  ),
  care_drugb = list(
    name = "Medicare Part B Drugs",
    alias = c(
      "drug_discard_ptb",
      "drug_spend_ptb"
    )
  ),
  care_drugd = list(
    name = "Medicare Part D Drugs",
    alias = c(
      "drug_opioid_ptd",
      "drug_spend_ptd"
    )
  ),
  care_market = list(
    name = "Market Saturation & Utilization",
    alias = c(
      "mkt_cbsa",
      "mkt_state"
    )
  ),
  care_in = list(
    name = "Medicare Inpatient Hospitals",
    alias = c(
      "in_geo",
      "in_prov",
      "in_serv"
    )
  ),
  care_out = list(
    name = "Medicare Outpatient Hospitals",
    alias = c(
      "out_geo",
      "out_serv"
    )
  ),
  care_prx = list(
    name = "Medicare Part D Prescribers",
    alias = c(
      "prx_geo",
      "prx_prov",
      "prx_drug"
    )
  ),
  care_dme = list(
    name = "Medicare DME, Devices & Supplies",
    alias = c(
      "dme_geo",
      "dme_prov",
      "dme_serv",
      "dme_supp",
      "dme_supserv"
    )
  ),
  care_pbj = list(
    name = "Nursing Home Payroll-Based Journal Staffing",
    alias = c(
      "pbj_non",
      "pbj_nurse",
      "pbj_emp"
    )
  ),
  care_util = list(
    name = "Medicare Physician & Other Practitioners",
    alias = c(
      "util_geo",
      "util_prov",
      "util_serv"
    )
  ),
  care_nhome = list(
    name = "Nursing Home Performance",
    alias = c(
      "nh_perf_tmp",
      "nh_mds_freq_tmp",
      "nh_mds_fac_tmp"
    )
  )
)
