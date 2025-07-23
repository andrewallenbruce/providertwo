#' @include aka_end.R
NULL

#' @autoglobal
#' @noRd
lf <- function(aka) {
  function(x) {
    grep(
      pattern = x,
      x       = names(aka),
      perl    = TRUE,
      value   = TRUE
    )
  }
}

#' @autoglobal
#' @noRd
look <- list(
  caid = lf(c(end_caid$current, end_caid$temporal)),
  prov = lf(end_prov$current),
  care = lf(c(end_care$current, end_care$temporal)),
  open = lf(c(end_open$current, end_open$temporal)),
  hgov = lf(c(end_hgov$current, end_hgov$temporal))
)

#' @autoglobal
#' @noRd
collect_caid = list(
  demos = list(
    name  = "Medicaid and CHIP Enrollee Demographics",
    alias = look$caid("^demo")),
  services = list(
    name  = "Services Provided to the Medicaid and CHIP Population",
    alias = look$caid("^service")),
  benes = list(
    name  = "Beneficiaries Receiving A Service",
    alias = look$caid("^benes_")),
  finance = list(
    name  = "Medicaid Financial Management Data",
    alias = look$caid("fin_")),
  nadac = list(
    name  = "NADAC (National Average Drug Acquisition Cost)",
    alias = look$caid("^nadac_")),
  pkg = list(
    name  = "Benefit Package for Medicaid and CHIP Beneficiaries",
    alias = look$caid("^pkg_")),
  drug = list(
    name  = "Medicaid Drug Datasets",
    alias = look$caid("^drug_")),
  dual = list(
    name  = "Dual Status Information for Medicaid and CHIP Beneficiaries",
    alias = look$caid("^dual_")),
  meg = list(
    name  = "Major Eligibility Group Information for Medicaid and CHIP Beneficiaries",
    alias = look$caid("^meg_")),
  cms64 = list(
    name  = "Medicaid CMS-64",
    alias = look$caid("^cms64_")),
  managed = list(
    name  = "Managed Care Enrollment",
    alias = look$caid("^man_")),
  unwind = list(
    name  = "Medicaid Unwinding Report",
    alias = look$caid("^wind_")))

#' @autoglobal
#' @noRd
collect_open = list(
  profile = list(
    name  = "Open Payments Profiles",
    alias = look$open("^prof_")),
  summary = list(
    name  = "Open Payments Summaries",
    alias = look$open("^sum_")),
  pay_group = list(
    name  = "Open Payments by Year (Grouped)",
    alias = look$open("^grp_")),
  pay_detail = list(
    name  = "Open Payments by Year (Detailed)",
    alias = look$open("^pay_")))

#' @autoglobal
#' @noRd
collect_prov = list(
  cahps_spice = list(
    name  = "CAHPS Hospice Survey Data",
    alias = look$prov("^cahps_spice_")),
  cahps_hhc = list(
    name  = "Home Health Care Patient Survey Data (HHCAHPS)",
    alias = look$prov("^cahps_hhc_")),
  cahps_ich = list(
    name  = "In-Center Hemodialysis Consumer Assessment Of Healthcare Providers And Services Systems (ICH CAHPS) Survey",
    alias = look$prov("^cahps_ich_")),
  oas_cahps = list(
    name  = "Outpatient and Ambulatory Surgery Consumer Assessment of Healthcare Providers and Systems (OAS CAHPS) Survey",
    alias = look$prov("^oas_cahps_")),
  mips = list(
    name  = "PY 2022 MIPS Public Reporting",
    alias = look$prov("^mips_")),
  pdc = list(
    name  = "Provider Data Catalog",
    alias = look$prov("^pdc_")),
  ltch = list(
    name  = "Long-Term Care Hospitals",
    alias = look$prov("^ltch_")),
  irf = list(
    name  = "Inpatient Rehabilitation Facilities",
    alias = look$prov("^irf_")),
  spice = list(
    name  = "Hospices",
    alias = look$prov("^spice_")),
  hhvbp = list(
    name  = "Expanded Home Health Value-Based Purchasing (HHVBP) Model",
    alias = look$prov("^hhvbp_")),
  home_health = list(
    name  = "Home Health Care Agencies",
    alias = look$prov("^hhc_")),
  snf_vbp = list(
    name  = "FY 2025 SNF VBP",
    alias = look$prov("^snf_vbp_")),
  snf_qrp = list(
    name  = "SNF Quality Measures",
    alias = look$prov("^snf_qrp_")),
  nh = list(
    name = "Nursing Homes",
    alias = look$prov("^nh_")),
  comp = list(
    name  = "Complications and Deaths",
    alias = look$prov("^comp_")),
  pch_comp = list(
    name  = "Complications and Unplanned Hospital Visits: PPS-Exempt Cancer Hospital",
    alias = look$prov("^pch_comp_")),
  asc = list(
    name  = "Ambulatory Surgical Center Quality Measures",
    alias = grep("^asc", names(end_prov$current), value = TRUE)),
  equity = list(
    name  = "Health Equity",
    alias = look$prov("^he_")),
  hai = list(
    name  = "Healthcare Associated Infections",
    alias = look$prov("^hai_")),
  dialysis = list(
    name  = "Dialysis Facilities",
    alias = look$prov("^dial_")),
  esrd = list(
    name  = "ESRD QIP",
    alias = look$prov("^esrd_")),
  hvbp = list(
    name  = "Hospital Value-Based Purchasing (HVBP)",
    alias = look$prov("^hvbp_")),
  ipf = list(
    name  = "Inpatient Psychiatric Facility Quality Measure Data",
    alias = look$prov("^ipf_")),
  mspb = list(
    name  = "Medicare Spending Per Beneficiary",
    alias = look$prov("^mspb_")),
  imaging = list(
    name  = "Outpatient Imaging Efficiency",
    alias = look$prov("^out_img_")),
  pch_pall = list(
    name  = "Palliative Care: PPS-Exempt Cancer Hospital",
    alias = look$prov("^pch_pall_")),
  pch_hcahps = list(
    name  = "Patient Survey (PCH HCAHPS) PPS-Exempt Cancer Hospital",
    alias = look$prov("^pch_hcahps_")),
  hcahps = list(
    name  = "Patient Survey (HCAHPS)",
    alias = look$prov("^hcahps_")),
  timely = list(
    name  = "Timely and Effective Care",
    alias = look$prov("^timely_")),
  visit = list(
    name  = "Unplanned Hospital Visits",
    alias = look$prov("^visit_")),
  va = list(
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
collect_care = list(
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

#' @autoglobal
#' @noRd
rlang::on_load({
  collection_names <- mph_init(names(c(
    collect_prov, collect_caid, collect_care, collect_open
  )))
  collection_regex <- c(collect_prov, collect_caid, collect_care, collect_open)
})

#' @autoglobal
#' @noRd
rex_collect <- function(x, call = caller_env()) {

  if (is.na(mph_match(x, collection_names))) {
    cli::cli_abort(
      c("x" = "{.val {x}} is not a valid collection."),
      call = call)
  }
  collection_regex[mph_match(x, collection_names)] |> unname() |> yank()
}
