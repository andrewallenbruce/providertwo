#' @include aka_end.R
NULL

#' @autoglobal
#' @noRd
fn_lk <- function(...) {
  function(x) {
    grep(pattern = x, x = rlang::names2(c(...)), perl = TRUE, value = TRUE)
  }
}

#' @autoglobal
#' @noRd
LU <- list(
  caid = fn_lk(end_caid$current, end_caid$temporal),
  care = fn_lk(end_care$current, end_care$temporal),
  hgov = fn_lk(end_hgov$current, end_hgov$temporal),
  open = fn_lk(end_open$current, end_open$temporal),
  prov = fn_lk(end_prov$current)
)

#' @autoglobal
#' @noRd
collect_caid = list(
  bene       = list(name = "Beneficiaries Receiving A Service", alias = LU$caid("^bene_")),
  demo       = list(name = "Medicaid and CHIP Enrollee Demographics", alias = LU$caid("^demo_")),
  service    = list(name = "Medicaid and CHIP Population Services", alias = LU$caid("^serv_")),
  finance    = list(name = "Medicaid Financial Management", alias = LU$caid("fin_")),
  nadac      = list(name = "NADAC (National Average Drug Acquisition Cost)", alias = LU$caid("^nadac_")),
  pkg        = list(name = "Benefit Package for Medicaid and CHIP Beneficiaries", alias = LU$caid("^pkg_")),
  drug       = list(name = "Medicaid Drug Datasets", alias = LU$caid("^drug_")),
  dual       = list(name = "Medicaid and CHIP Dual Status Beneficiaries", alias = LU$caid("^dual_")),
  meg        = list(name = "Major Eligibility Group Information for Medicaid and CHIP Beneficiaries", alias = LU$caid("^meg_")),
  cms64      = list(name = "Medicaid CMS-64", alias = LU$caid("^cms64_")),
  managed    = list(name = "Managed Care Enrollment", alias = LU$caid("^man_")),
  unwind     = list(name = "Medicaid Unwinding Report", alias = LU$caid("^wind_")))

#' @autoglobal
#' @noRd
collect_open = list(
  profile    = list(name = "Open Payments Profiles", alias = LU$open("^prf_")),
  summary    = list(name = "Open Payments Summaries", alias = LU$open("^sum_")),
  pay_group  = list(name = "Open Payments by Year (Grouped)", alias = LU$open("^grp_")),
  pay_detail = list(name = "Open Payments by Year (Detailed)", alias = LU$open("^pay_")))

#' @autoglobal
#' @noRd
collect_prov  = list(
  asc         = list(name = "ASC Quality Measures", alias = LU$prov("^asc_")),
  cahps_hhc   = list(name = "HHCAHPS Patient Survey", alias = LU$prov("^cahps_hhc_")),
  cahps_hosp  = list(name = "HCAHPS Patient Survey", alias = LU$prov("^hcahps_")),
  cahps_ich   = list(name = "ICH CAHPS Survey", alias = LU$prov("^cahps_ich_")),
  cahps_oas   = list(name = "OAS CAHPS Survey", alias = LU$prov("^oas_cahps_")),
  cahps_spice = list(name = "CAHPS Hospice Survey", alias = LU$prov("^cahps_spice_")),
  cmp_death   = list(name = "Complications and Deaths", alias = LU$prov("^comp_")),
  dialysis    = list(name = "Dialysis Facilities", alias = LU$prov("^dial_")),
  esrd        = list(name = "ESRD QIP", alias = LU$prov("^esrd_")),
  hac_hrr     = list(name = "HAC & HRR Programs", alias = LU$prov("^reduc_")),
  hai         = list(name = "Healthcare-Associated Infections", alias = LU$prov("^hai_")),
  heq         = list(name = "Health Equity", alias = LU$prov("^he_")),
  hhc         = list(name = "Home Health Care Agencies", alias = LU$prov("^hhc_")),
  hhvbp       = list(name = "Home Health Value-Based Purchasing", alias = LU$prov("^hhvbp_")),
  hvbp        = list(name = "Hospital Value-Based Purchasing", alias = LU$prov("^hvbp_")),
  imaging     = list(name = "Outpatient Imaging Efficiency", alias = LU$prov("^out_img_")),
  ipf         = list(name = "Inpatient Psychiatric Facility Quality Measures", alias = LU$prov("^ipf_")),
  irf         = list(name = "Inpatient Rehabilitation Facilities", alias = LU$prov("^irf_")),
  ltch        = list(name = "Long-Term Care Hospitals", alias = LU$prov("^ltch_")),
  mips        = list(name = "MIPS Public Reporting", alias = LU$prov("^mips_")),
  mspb        = list(name = "Medicare Spending Per Beneficiary", alias = LU$prov("^mspb_")),
  nh          = list(name = "Nursing Homes", alias = LU$prov("^nh_")),
  pch_comp    = list(name = "Complications/Unplanned Visits (PCH)", alias = LU$prov("^pch_comp_")),
  pch_hcahps  = list(name = "HCAHPS (PCH)", alias = LU$prov("^pch_hcahps_")),
  pch_pall    = list(name = "Palliative Care (PCH)", alias = LU$prov("^pch_pall_")),
  pdc         = list(name = "Provider Data Catalog", alias = LU$prov("^pdc_")),
  snf_vbp     = list(name = "SNF Value-Based Purchasing", alias = LU$prov("^snf_vbp_")),
  snf_qrp     = list(name = "SNF Quality Measures", alias = LU$prov("^snf_qrp_")),
  spice_p     = list(name = "Hospices", alias = LU$prov("^spice_")),
  table       = list(name = "Hospital Changes in Payment", alias = LU$prov("^tbl_")),
  timely      = list(name = "Timely and Effective Care", alias = LU$prov("^timely_")),
  vets        = list(name = "Veterans Health Administration", alias = LU$prov("^va_")),
  visit       = list(name = "Unplanned Hospital Visits", alias = LU$prov("^visit_")),
  voc         = list(name = "Payment and Value of Care", alias = LU$prov("^voc_|^pmt_"))
)

#' @autoglobal
#' @noRd
collect_care = list(
  hha        = list(name = "Home Health Agencies", alias = LU$care("^hha_")),
  spice_c    = list(name = "Hospices", alias = LU$care("^spice_")),
  hosp       = list(name = "Hospitals", alias = LU$care("^hosp_")),
  rhc        = list(name = "Rural Health Clinics", alias = LU$care("^rhc_")),
  fqhc       = list(name = "Federally Qualified Health Centers", alias = LU$care("^fqhc_")),
  pend       = list(name = "Pending Initial Logging and Tracking", alias = LU$care("^pilat_")),
  reval      = list(name = "Revalidation Reassignments", alias = LU$care("^reval_")),
  snf        = list(name = "Skilled Nursing Facilities", alias = LU$care("^snf_")),
  aco        = list(name = "Accountable Care Organizations", alias = LU$care("^aco_")),
  reach      = list(name = "Accountable Care REACH Organizations", alias = LU$care("^reach_")),
  care_caid  = list(name = "Medicare Medicaid", alias = LU$care("_caid$")),
  geo        = list(name = "Medicare Geographic Variation", alias = LU$care("^geo_")),
  pdp        = list(name = "Pharmacy Network/Formulary/Pricing", alias = LU$care("^pdp_")),
  bene_surv  = list(name = "Medicare Current Beneficiary Survey", alias = LU$care("^survey_")),
  drugb      = list(name = "Medicare Part B Drugs", alias = LU$care("^drugb_")),
  drugd      = list(name = "Medicare Part D Drugs", alias = LU$care("^drugd_")),
  mkt        = list(name = "Market Saturation & Utilization", alias = LU$care("^mkt_")),
  in_hosp    = list(name = "Medicare Inpatient Hospitals", alias = LU$care("^in_")),
  out_hosp   = list(name = "Medicare Outpatient Hospitals", alias = LU$care("^out_")),
  prx        = list(name = "Medicare Part D Prescribers", alias = LU$care("^prx_")),
  dme        = list(name = "Medicare DME, Devices & Supplies", alias = LU$care("^dme_")),
  pbj        = list(name = "Nursing Home Payroll-Based Journal Staffing", alias = LU$care("^pbj_")),
  util       = list(name = "Medicare Physician & Other Practitioners", alias = LU$care("^util_")),
  mds_nh     = list(name = "Nursing Home Performance", alias = LU$care("^mds_nh_")),
  innovate   = list(name = "Innovation Center", alias = LU$care("^inno_"))
)

#' @autoglobal
#' @noRd
rlang::on_load({
  collection_regex <- c(collect_prov, collect_caid, collect_care, collect_open)
  collection_names <- minit(collection_regex)
})

#' @autoglobal
#' @noRd
is_collection <- function(x) {
  !cheapr::is_na(oomph::mph_match(x, collection_names))
}

#' @autoglobal
#' @noRd
any_collection <- function(x) {
  any(is_collection(x), na.rm = TRUE)
}

#' @autoglobal
#' @noRd
rex_collect <- function(x, call = rlang::caller_env()) {
  collection_regex[oomph::mph_match(x, collection_names)] |> unname() |> yank()
}
