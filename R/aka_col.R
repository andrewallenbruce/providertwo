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
  demos      = list(
    name     = "Medicaid and CHIP Enrollee Demographics",
    alias    = look$caid("^demo")),
  services   = list(
    name     = "Services Provided to the Medicaid and CHIP Population",
    alias    = look$caid("^service")),
  benes      = list(
    name     = "Beneficiaries Receiving A Service",
    alias    = look$caid("^benes_")),
  finance    = list(
    name     = "Medicaid Financial Management Data",
    alias    = look$caid("fin_")),
  nadac      = list(
    name     = "NADAC (National Average Drug Acquisition Cost)",
    alias    = look$caid("^nadac_")),
  pkg        = list(
    name     = "Benefit Package for Medicaid and CHIP Beneficiaries",
    alias    = look$caid("^pkg_")),
  drug       = list(
    name     = "Medicaid Drug Datasets",
    alias    = look$caid("^drug_")),
  dual       = list(
    name     = "Dual Status Information for Medicaid and CHIP Beneficiaries",
    alias    = look$caid("^dual_")),
  meg        = list(
    name     = "Major Eligibility Group Information for Medicaid and CHIP Beneficiaries",
    alias    = look$caid("^meg_")),
  cms64      = list(
    name     = "Medicaid CMS-64",
    alias    = look$caid("^cms64_")),
  managed    = list(
    name     = "Managed Care Enrollment",
    alias    = look$caid("^man_")),
  unwind     = list(
    name     = "Medicaid Unwinding Report",
    alias    = look$caid("^wind_")))

#' @autoglobal
#' @noRd
collect_open = list(
  profile    = list(
    name     = "Open Payments Profiles",
    alias    = look$open("^prof_")),
  summary    = list(
    name     = "Open Payments Summaries",
    alias    = look$open("^sum_")),
  pay_group  = list(
    name     = "Open Payments by Year (Grouped)",
    alias    = look$open("^grp_")),
  pay_detail = list(
    name     = "Open Payments by Year (Detailed)",
    alias    = look$open("^pay_")))

#' @autoglobal
#' @noRd
collect_prov  = list(
  cahps_spice = list(
    name      = "CAHPS Hospice Survey Data",
    alias     = look$prov("^cahps_spice_")),
  cahps_hhc   = list(
    name      = "Home Health Care Patient Survey Data (HHCAHPS)",
    alias     = look$prov("^cahps_hhc_")),
  cahps_ich   = list(
    name      = "In-Center Hemodialysis Consumer Assessment Of Healthcare Providers And Services Systems (ICH CAHPS) Survey",
    alias     = look$prov("^cahps_ich_")),
  oas_cahps   = list(
    name      = "Outpatient and Ambulatory Surgery Consumer Assessment of Healthcare Providers and Systems (OAS CAHPS) Survey",
    alias     = look$prov("^oas_cahps_")),
  mips        = list(
    name      = "PY 2022 MIPS Public Reporting",
    alias     = look$prov("^mips_")),
  pdc         = list(
    name      = "Provider Data Catalog",
    alias     = look$prov("^pdc_")),
  ltch        = list(
    name      = "Long-Term Care Hospitals",
    alias     = look$prov("^ltch_")),
  irf         = list(
    name      = "Inpatient Rehabilitation Facilities",
    alias     = look$prov("^irf_")),
  p_spice       = list(
    name      = "Hospices",
    alias     = look$prov("^spice_")),
  hhvbp       = list(
    name      = "Expanded Home Health Value-Based Purchasing (HHVBP) Model",
    alias     = look$prov("^hhvbp_")),
  hhc         = list(
    name      = "Home Health Care Agencies",
    alias     = look$prov("^hhc_")),
  snf_vbp     = list(
    name      = "FY 2025 SNF VBP",
    alias     = look$prov("^snf_vbp_")),
  snf_qrp     = list(
    name      = "SNF Quality Measures",
    alias     = look$prov("^snf_qrp_")),
  nh          = list(
    name      = "Nursing Homes",
    alias     = look$prov("^nh_")),
  comp        = list(
    name      = "Complications and Deaths",
    alias     = look$prov("^comp_")),
  pch_comp    = list(
    name      = "Complications and Unplanned Hospital Visits: PPS-Exempt Cancer Hospital",
    alias     = look$prov("^pch_comp_")),
  asc         = list(
    name      = "Ambulatory Surgical Center Quality Measures",
    alias     = look$prov("^asc_")),
  equity      = list(
    name      = "Health Equity",
    alias     = look$prov("^he_")),
  hai         = list(
    name      = "Healthcare Associated Infections",
    alias     = look$prov("^hai_")),
  dialysis    = list(
    name      = "Dialysis Facilities",
    alias     = look$prov("^dial_")),
  esrd        = list(
    name      = "ESRD QIP",
    alias     = look$prov("^esrd_")),
  hvbp        = list(
    name      = "Hospital Value-Based Purchasing (HVBP)",
    alias     = look$prov("^hvbp_")),
  ipf         = list(
    name      = "Inpatient Psychiatric Facility Quality Measure Data",
    alias     = look$prov("^ipf_")),
  mspb        = list(
    name      = "Medicare Spending Per Beneficiary",
    alias     = look$prov("^mspb_")),
  imaging     = list(
    name      = "Outpatient Imaging Efficiency",
    alias     = look$prov("^out_img_")),
  pch_pall    = list(
    name      = "Palliative Care: PPS-Exempt Cancer Hospital",
    alias     = look$prov("^pch_pall_")),
  pch_hcahps  = list(
    name      = "Patient Survey (PCH HCAHPS) PPS-Exempt Cancer Hospital",
    alias     = look$prov("^pch_hcahps_")),
  hcahps      = list(
    name      = "Patient Survey (HCAHPS)",
    alias     = look$prov("^hcahps_")),
  timely      = list(
    name      = "Timely and Effective Care",
    alias     = look$prov("^timely_")),
  visit       = list(
    name      = "Unplanned Hospital Visits",
    alias     = look$prov("^visit_")),
  va          = list(
    name      = "Veterans Health Administration",
    alias     = look$prov("^va_")),
  table       = list(
    name      = "Hospital FY2021 Changes in Payment",
    alias     = look$prov("^tbl_")),
  voc         = list(
    name      = "Payment and Value of Care",
    alias     = look$prov("^voc_|^pmt_")),
  reduction   = list(
    name      = "Hospital-Acquired Condition & Readmission Reduction Programs",
    alias     = look$prov("^reduc_"))
)

# ---- prov ----
#' @autoglobal
#' @noRd
collect_care = list(
  hha        = list(
    name     = "Home Health Agencies",
    alias    = look$care("^hha_")),
  c_spice    = list(
    name     = "Hospices",
    alias    = look$care("^spice_")),
  hosp       = list(
    name     = "Hospitals",
    alias    = look$care("^hosp_")),
  rhc        = list(
    name     = "Rural Health Clinics",
    alias    = look$care("^rhc_")),
  fqhc       = list(
    name     = "Federally Qualified Health Centers",
    alias    = look$care("^fqhc_")),
  pend       = list(
    name     = "Pending Initial Logging and Tracking",
    alias    = look$care("^pilat_")),
  reval      = list(
    name     = "Revalidation Reassignments",
    alias    = look$care("^reval_")),
  snf        = list(
    name     = "Skilled Nursing Facilities",
    alias    = look$care("^snf_")),
  aco        = list(
    name     = "Accountable Care Organizations",
    alias    = look$care("^aco_")),
  reach_aco  = list(
    name     = "Accountable Care REACH Organizations",
    alias    = look$care("^reach_")),
  care_caid  = list(
    name     = "Medicare Medicaid",
    alias    = look$care("_caid$")),
  geo        = list(
    name     = "Medicare Geographic Variation",
    alias    = look$care("^geo_")),
  pdp        = list(
    name     = "Pharmacy Network/Formulary/Pricing",
    alias    = look$care("^pdp_")),
  bene_surv  = list(
    name     = "Medicare Current Beneficiary Survey",
    alias    = look$care("^survey_")),
  drugb      = list(
    name     = "Medicare Part B Drugs",
    alias    = look$care("^drugb_")),
  drugd      = list(
    name     = "Medicare Part D Drugs",
    alias    = look$care("^drugd_")),
  mkt        = list(
    name     = "Market Saturation & Utilization",
    alias    = look$care("^mkt_")),
  in_hosp    = list(
    name     = "Medicare Inpatient Hospitals",
    alias    = look$care("^in_")),
  out_hosp   = list(
    name     = "Medicare Outpatient Hospitals",
    alias    = look$care("^out_")),
  prx        = list(
    name     = "Medicare Part D Prescribers",
    alias    = look$care("^prx_")),
  dme        = list(
    name     = "Medicare DME, Devices & Supplies",
    alias    = look$care("^dme_")),
  pbj        = list(
    name     = "Nursing Home Payroll-Based Journal Staffing",
    alias    = look$care("^pbj_")),
  util       = list(
    name     = "Medicare Physician & Other Practitioners",
    alias    = look$care("^util_")),
  nh_perf    = list(
    name     = "Nursing Home Performance",
    alias    = look$care("^mds_nh_"))
)

#' @autoglobal
#' @noRd
rlang::on_load({
  collection_names <- mph_init(
    names(c(collect_prov, collect_caid, collect_care, collect_open))
  )
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
