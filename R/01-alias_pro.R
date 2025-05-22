#' @include S7_classes.R
NULL

#' Provider API Endpoints
#' @name provider
#' @param alias `<chr>` endpoint or group alias
#' @param call `<env>` environment to use for error reporting
#' @param ... Additional arguments passed to the group constructor
#' @returns An S7 `<class_endpoint>` or `<class_group>` object
#' @examples
#' pro_endpoint("asc_facility")
#' pro_group("pro_dialysis")
NULL

#' @autoglobal
#' @rdname provider
#' @export
pro_endpoint <- function(alias, call = caller_env()) {
  x <- switch(
    alias,
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
    dialysis_byfacility       = "^Dialysis Facility [-] Listing by Facility$",
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
    va_timely                 = "^Veterans Health Administration Timely and Effective Care Data",
    cli::cli_abort(c("x"      = "{.emph alias} {.val {alias}} is invalid."), call = call)
  )

  res <- select_alias(the$catalogs$pro$end, x)

  if (empty(res))     cli::cli_abort(c("x" = "{.val {x}} returned no matches."), call = call)
  if (nrow(res) > 1L) cli::cli_abort(c("x" = "{.val {x}} returned more than 1 match."), call = call)

  x <- c(res)

  class_endpoint(
    identifier  = x$identifier,
    metadata    = get_metadata(x),
    dimensions  = get_dimensions(x)
  )
}

#' @autoglobal
#' @rdname provider
#' @export
pro_group <- function(alias, call = caller_env(), ...) {
  x <- switch(
    alias,
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
    pro_dialysis         = list(group = "Dialysis Facilities", alias = c("dialysis_facility", "dialysis_byfacility", "dialysis_national", "dialysis_state")),
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
    cli::cli_abort(c("x" = "{.emph alias} {.val {alias}} is invalid."), call = call)
  )

  new_group(
    member_names = x$alias,
    group_name   = x$group,
    ...
  )
}
