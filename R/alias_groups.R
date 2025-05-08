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
        "SPICE_owners",
        "SPICE_enrollments",
        "SPICE_acute"
      )
    ),
    hospital = list(
      group = "Hospitals",
      alias = c(
        "hospital_owners",
        "hospital_chow",
        "hospital_chow_owner",
        "hospital_enrollments",
        "hospital_cost_report",
        "hospital_service_area"
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
        "REVAL_reassign"
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
    ACO = list(
      group = "Accountable Care Organizations",
      alias = c(
        "ACO_pioneer",
        "ACO_participants",
        "ACO_SNF",
        "ACO_orgs",
        "ACO_bene",
        "ACO_REACH_align",
        "ACO_REACH_elig",
        "ACO_REACH_fin",
        "ACO_REACH_prov",
        "ACO_REACH_orgs"
      )
    ),
    program_stats = list(
      group = "CMS Program Statistics",
      alias = c(
        "CMS_MA_enroll",
        "CMS_MA_outpatient",
        "CMS_MA_other",
        "CMS_MA_inpatient",
        "CMS_MA_SNF",
        "CMS_deaths",
        "CMS_HHA",
        "CMS_spice",
        "CMS_inpatient",
        "CMS_new_enroll",
        "CMS_outpatient",
        "CMS_TOS",
        "CMS_partD",
        "CMS_partD_enroll",
        "CMS_other",
        "CMS_premiums",
        "CMS_providers",
        "CMS_SNF",
        "CMS_total_enroll",
        "CMS_dual_enroll",
        "CMS_orig_enroll"
      )
    ),
    CAID = list(
      group = "Medicaid",
      alias = c(
        "CAID_managed",
        "CAID_opioid",
        "CAID_drug"
      )
    ),
    geo_variation = list(
      group = "Medicare Geographic Variation",
      alias = c(
        "GEO_MA",
        "GEO_HRR",
        "GEO_NSC"
      )
    ),
    PDP = list(
      group = "Prescription Drug Plan Formulary, Pharmacy Network, and Pricing Information",
      alias = c(
        "PDP_month",
        "PDP_quarter"
      )
    ),
    BENE_surv = list(
      group = "Medicare Current Beneficiary Survey",
      alias = c(
        "BENE_surv_covid",
        "BENE_surv_cost",
        "BENE_surv_file"
      )
    ),
    PTB_drugs = list(
      group = "Medicare Part B Drugs",
      alias = c(
        "PTB_discard",
        "PTB_spending"
      )
    ),
    MKT = list(
      group = "Market Saturation & Utilization",
      alias = c(
        "MKT_cbsa",
        "MKT_state_cnty"
      )
    ),
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
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
        "IN_geography",
        "IN_provider",
        "IN_service"
      )
    ),
    outpatient  = list(
      group = "Medicare Outpatient Hospitals",
      alias = c(
        "OUT_geography",
        "OUT_service"
      )
    ),
    prescribers = list(
      group = "Medicare Part D Prescribers",
      alias = c(
        "PRX_geography",
        "PRX_provider",
        "PRX_drug"
      )
    ),
    suppliers   = list(
      group = "Medicare Durable Medical Equipment, Devices & Supplies",
      alias = c(
        "DME_geography",
        "DME_provider",
        "DME_service",
        "DME_supplier",
        "DME_supplier_service"
      )
    ),
    staffing    = list(
      group = "Nursing Home Payroll-Based Journal Staffing",
      alias = c(
        "NH_staff_nonurse",
        "NH_staff_nurse",
        "NH_staff_employee"
      )
    ),
    utilization = list(
      group = "Medicare Physician & Other Practitioners",
      alias = c(
        "UTIL_geography",
        "UTIL_provider",
        "UTIL_service"
      )
    ),
    care_nursing = list(group = "Nursing Home Performance", alias = c(
        "NH_performance",
        "NH_mds_frequency",
        "NH_mds_facility"
      )
    ),
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
}

#' @autoglobal
#' @noRd
select_pro_group <- function(x, call = caller_env()) {
  switch(
    x,
    pro_cahps_spice      = list(group = "CAHPS Hospice Survey Data", alias = c("cahps_hospice_nation", "cahps_hospice_provider", "cahps_hospice_state")),
    pro_cahps_hhc        = list(group = "Home Health Care Patient Survey Data (HHCAHPS)", alias = c("cahps_hhc_patient", "cahps_hhc_measure", "cahps_hhc_national", "cahps_hhc_state")),
    pro_cahps_ich        = list(group = "In-Center Hemodialysis Consumer Assessment Of Healthcare Providers And Services Systems (ICH CAHPS) Survey", alias = c("cahps_ich_esrd", "cahps_ich_facility", "cahps_ich_national", "cahps_ich_state")),
    pro_cahps_oas        = list(group = "Outpatient and Ambulatory Surgery Consumer Assessment of Healthcare Providers and Systems (OAS CAHPS) Survey", alias = c("cahps_oas_footnotes", "cahps_oas_asc_facility", "cahps_oas_asc_national", "cahps_oas_asc_state", "cahps_oas_hosp_facility", "cahps_oas_hosp_national", "cahps_oas_hosp_state")),
    pro_mips             = list(group = "PY 2022 MIPS Public Reporting", alias = c("mips_performance", "mips_patient", "mips_clinician", "mips_group", "mips_virtual")),
    pro_drs              = list(group = "Provider Data Catalog", alias = c("pdc_affiliations", "pdc_clinicians", "pdc_utilization")),
    pro_ltch             = list(group = "Long-Term Care Hospitals", alias = c("ltch_general", "ltch_provider", "ltch_national")),
    pro_irf              = list(group = "Inpatient Rehabilitation Facilities", alias = c("irf_conditions", "irf_general", "irf_provider", "irf_national")),
    pro_hospice          = list(group = "Hospices", alias = c("hospice_general", "hospice_provider", "hospice_state", "hospice_zip", "hospice_national")),
    pro_hhc_vbp          = list(group = "Expanded Home Health Value-Based Purchasing (HHVBP) Model", alias = c("hhvbp_agency", "hhvbp_cohort")),
    pro_home_health      = list(group = "Home Health Care Agencies", alias = c("hhc_range", "hhc_national", "hhc_state", "hhc_zip", "hhc_agency")),
    pro_snf_vbp          = list(group = "FY 2025 SNF VBP", alias = c("snf_vbp_performance", "snf_vbp_facility")),
    pro_snf_quality      = list(group = "SNF Quality Measures", alias = c("snf_quality_nation", "snf_quality_provider", "snf_quality_swing")),
    pro_nursing          = list(group = "Nursing Homes", alias = c("nursing_ownership", "nursing_penalties", "nursing_provider", "nursing_citation", "nursing_fire", "nursing_deficiencies", "nursing_inspection", "nursing_quality_mds", "nursing_quality_claims", "nursing_state_avg", "nursing_state_cut", "nursing_interval")),
    pro_complication     = list(group = "Complications and Deaths", alias = c("complication_hospital", "complication_state", "complication_national")),
    pro_complication_pch = list(group = "Complications and Unplanned Hospital Visits: PPS-Exempt Cancer Hospital", alias = c("complication_pch_hospital", "complication_pch_national")),
    pro_asc_quality      = list(group = "Ambulatory Surgical Center Quality Measures", alias = c("asc_facility", "asc_national", "asc_state")),
    pro_equity           = list(group = "Health Equity", alias = c("he_hospital", "he_national", "he_state")),
    pro_hai              = list(group = "Healthcare Associated Infections", alias = c("hai_hospital", "hai_national", "hai_state", "hai_PCH")),
    pro_dialysis         = list(group = "Dialysis Facilities", alias = c("dialysis_facility", "dialysis_national", "dialysis_state")),
    pro_esrd             = list(group = "ESRD QIP", alias = c("esrd_depression", "esrd_complete", "esrd_adequacy", "esrd_footnotes", "esrd_hypercalcemia", "esrd_medication", "esrd_infection", "esrd_event", "esrd_waitlist", "esrd_hospitalization", "esrd_readmission", "esrd_transfusion", "esrd_performance", "esrd_ultrafiltration", "esrd_vascular")),
    pro_hvbp             = list(group = "Hospital Value-Based Purchasing (HVBP)", alias = c("hvbp_outcomes", "hvbp_efficiency", "hvbp_engagement", "hvbp_safety", "hvbp_performance")),
    pro_ipf              = list(group = "Inpatient Psychiatric Facility Quality Measure Data", alias = c("ipf_national", "ipf_facility", "ipf_state")),
    pro_mspb             = list(group = "Medicare Spending Per Beneficiary", alias = c("mspb_claim", "mspb_hospital", "mspb_decimal", "mspb_national", "mspb_state")),
    pro_out_img          = list(group = "Outpatient Imaging Efficiency", alias = c("out_img_hospital", "out_img_national", "out_img_state")),
    pro_pch_pall         = list(group = "Palliative Care: PPS-Exempt Cancer Hospital", alias = c("pch_pall_hospital", "pch_pall_national")),
    pro_pch_hcahps       = list(group = "Patient Survey (PCH HCAHPS) PPS-Exempt Cancer Hospital", alias = c("hcahps_pch_hospital", "hcahps_pch_national", "hcahps_pch_state")),
    pro_hcahps           = list(group = "Patient Survey (HCAHPS)", alias = c("hcahps_hospital", "hcahps_national", "hcahps_state")),
    pro_timely           = list(group = "Timely and Effective Care", alias = c("timely_hospital", "timely_national", "timely_state")),
    pro_unplan           = list(group = "Unplanned Hospital Visits", alias = c("unplan_hospital", "unplan_national", "unplan_state")),
    pro_va               = list(group = "Veterans Health Administration", alias = c("va_behavioral", "va_provider", "va_timely")),
    pro_hospital_changes = list(group = "Hospital FY2021 Changes in Payment", alias = c("hospital_drg_net", "hospital_drg_dist", "hospital_pmt_pct", "hospital_pmt_vbi")),
    pro_hospital_voc     = list(group = "Payment and Value of Care", alias = c("hospital_voc_nation", "hospital_voc_hosp", "hospital_pmt_state", "hospital_pmt_nation")),
    pro_reduction        = list(group = "Hospital-Acquired Condition & Readmission Reduction Programs", alias = c("reduction_hac", "reduction_hrr")),

    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
}

#' @autoglobal
#' @noRd
select_caid_group <- function(x, call = caller_env()) {
  switch(
    x,
    demographics = list(
      group = "Medicaid and CHIP Enrollee Demographics",
      alias = c(
        "DEMO_well",
        "DEMO_sud",
        "DEMO_disability",
        "DEMO_premature",
        "DEMO_language",
        "DEMO_race",
        "DEMO_rural",
        "DEMO_waive"
      )
    ),
    services = list(
      group = "Services Provided to the Medicaid and CHIP Population",
      alias = c(
        "SERV_acute",
        "SERV_behavior",
        "SERV_perinatal",
        "SERV_screen",
        "SERV_contra",
        "SERV_dental",
        "SERV_pregnancy",
        "SERV_telehealth",
        "SERV_vaccination",
        "SERV_respiratory",
        "SERV_lead"
      )
    ),
    beneficiaries = list(
      group = "Beneficiaries Receiving A Service",
      alias = c(
        "BENE_behavior",
        "BENE_physical",
        "BENE_mental",
        "BENE_integrated",
        "BENE_nas",
        "BENE_smm",
        "BENE_postpart",
        "BENE_delivery"
      )
    ),
    financial = list(
      group = "Medicaid Financial Management Data",
      alias = c(
        "FIN_mgmt",
        "FIN_nation"
      )
    ),
    NADAC_group = list(
      group = "NADAC (National Average Drug Acquisition Cost)",
      alias = c(
        "NADAC",
        "NADAC_first",
        "NADAC_compare"
      )
    ),
    PKG = list(
      group = "Benefit Package for Medicaid and CHIP Beneficiaries",
      alias = c(
        "PKG_month",
        "PKG_year"
      )
    ),
    DRUG = list(
      group = "Medicaid Drug Datasets",
      alias = c(
        "DRUG_amp_mon",
        "DRUG_amp_qtr",
        "DRUG_products",
        "DRUG_clot",
        "DRUG_pediatric",
        "DRUG_contact_manu",
        "DRUG_contact_state"
      )
    ),
    DUAL = list(
      group = "Dual Status Information for Medicaid and CHIP Beneficiaries",
      alias = c(
        "DUAL_month",
        "DUAL_year"
      )
    ),
    MEGI = list(
      group = "Major Eligibility Group Information for Medicaid and CHIP Beneficiaries",
      alias = c(
        "MEG_month",
        "MEG_year"
      )
    ),
    CMS64 = list(
      group = "Medicaid CMS-64",
      alias = c(
        "CMS64_CAA",
        "CMS64_FFCRA",
        "CMS64_adult"
      )
    ),
    MC = list(
      group = "Managed Care Enrollment",
      alias = c(
        "MC_summary",
        "MC_program_state",
        "MC_program_plan",
        "MC_program_pop_all",
        "MC_program_pop_dual",
        "MC_feat_pop",
        "MC_feat_qa",
        "MC_bene_month",
        "MC_bene_year"
      )
    ),
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
}

#' @autoglobal
#' @noRd
select_open_group <- function(x, call = caller_env()) {
  switch(
    x,
    profile = list(
      group = "Open Payments Profiles",
      alias = c(
        "PROF_covered",
        "PROF_physician",
        "PROF_information",
        "PROF_mapping",
        "PROF_entity",
        "PROF_teaching"
      )
    ),
    summary = list(
      group = "Open Payments Summaries",
      alias = c(
        "SUMM_dashboard",
        "SUMM_state_all",
        "SUMM_state_group",
        "SUMM_nation_all",
        "SUMM_nation_group"
      )
    ),
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
}

#' @autoglobal
#' @noRd
select_open_troup <- function(x, call = caller_env()) {
  switch(
    x,
    grouped_payment = list(
      group = "Payments Grouped by Year",
      alias = c(
        "GROUP_recip_nature",
        "GROUP_recip_entity",
        "GROUP_entity_nature",
        "GROUP_all"
      )
    ),
    detailed_payment = list(
      group = "Payments Detailed by Year",
      alias = c(
        "DATA_general",
        "DATA_ownership",
        "DATA_research"
      )
    ),
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
}
