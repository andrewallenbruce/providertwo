#' @autoglobal
#' @noRd
select_pro <- function(x, call = caller_env()) {
  x <- switch(
    x,
    suppliers = "^Medical Equipment Suppliers$",
    PSI90_6digit = "CMS Medicare PSI-90 and component measures - six-digit estimate dataset",
    joint_provider = "Comprehensive Care For Joint Replacement Model - Provider Data",
    Data_Updates = "Data Updates",
    Footnote_Crosswalk = "Footnote Crosswalk",
    Hospital_General_Information = "Hospital General Information",
    Maternal_Health_Hospital = "Maternal Health - Hospital",
    Measure_Dates = "Measure Dates",
    Hospital_Patient_Outcomes = "Patient-Reported Outcomes - Hospital",
    Payment_National = "Payment - National",
    Payment_State = "Payment - State",
    Payment_Hospital = "Payment and value of care - Hospital",
    PI_Hospital = "Promoting Interoperability - Hospital",
    HAI_cancer_hospital = "Safety and Healthcare-Associated Infection Measures - PPS-Exempt Cancer Hospital",
    TBL_drg_net = "Table 1: FY2021 Net Change in Base Operating DRG Payment Amount",
    TBL_drg_dist = "Table 2: FY2021 Distribution of Net Change in Base Operating DRG Payment Amount",
    TBL_pmt_pct = "Table 3: FY2021 Percent Change in Medicare Payments",
    TBL_pmt_vbi = "Table 4: FY2021 Value-Based Incentive Payment Amount",
    Value_National = "Value of care - National",

    ASC_facility = "Ambulatory Surgical Center Quality Measures - Facility",
    ASC_national = "Ambulatory Surgical Center Quality Measures - National",
    ASC_state = "Ambulatory Surgical Center Quality Measures - State",

    COMP_hospital = "Complications and Deaths - Hospital",
    COMP_national = "Complications and Deaths - National",
    COMP_state = "Complications and Deaths - State",
    COMP_cancer_hospital = "Complications and Unplanned Hospital Visits - PPS-Exempt Cancer Hospital - Hospital",
    COMP_cancer_national = "Complications and Unplanned Hospital Visits - PPS-Exempt Cancer Hospital - National",
    EQUI_hospital = "Health Equity - Hospital",
    EQUI_national = "Health Equity - National",
    EQUI_state = "Health Equity - State",
    HAI_hospital = "Healthcare Associated Infections - Hospital",
    HAI_national = "Healthcare Associated Infections - National",
    HAI_state = "Healthcare Associated Infections - State",

    PDC_affiliations        = "^Facility Affiliation Data$",
    PDC_clinicians          = "^National Downloadable File$",
    PDC_utilization         = "^Utilization Data$",
    MIPS_performance        = "^PY 2022 Clinician Public Reporting: Overall MIPS Performance$",
    MIPS_patient            = "^PY 2022 Group Public Reporting: Patient Experience$",
    MIPS_clinician          = "^PY 2022 Clinician Public Reporting: MIPS Measures and Attestations$",
    MIPS_group              = "^PY 2022 Group Public Reporting: MIPS Measures and Attestations$",
    MIPS_virtual            = "^PY 2022 Virtual Group Public Reporting: MIPS Measures and Attestations$",
    LTCH_general            = "^Long-Term Care Hospital - General Information$",
    LTCH_provider           = "^Long-Term Care Hospital - Provider Data$",
    LTCH_national           = "^Long-Term Care Hospital - National Data$",
    IRF_conditions          = "^Inpatient Rehabilitation Facility - Conditions$",
    IRF_general             = "^Inpatient Rehabilitation Facility - General Information$",
    IRF_provider            = "^Inpatient Rehabilitation Facility - Provider Data$",
    IRF_national            = "^Inpatient Rehabilitation Facility - National Data$",
    SPICE_general           = "^Hospice - General Information$",
    SPICE_provider          = "^Hospice - Provider Data$",
    SPICE_state             = "^Hospice - State Data$",
    SPICE_zip               = "^Hospice - Zip Data$",
    SPICE_national          = "^Hospice-National Data$",
    HHVBP_agency            = "^Expanded Home Health Value-Based Purchasing \\(HHVBP\\) Model - Agency Data$",
    HHVBP_cohort            = "^Expanded Home Health Value-Based Purchasing \\(HHVBP\\) Model - Cohort Data$",
    HHC_range               = "^Home Health Care - Measure Date Range$",
    HHC_national            = "^Home Health Care - National Data$",
    HHC_state               = "^Home Health Care - State by State Data$",
    HHC_zip                 = "^Home Health Care - Zip Codes$",
    HHC_agency              = "^Home Health Care Agencies$",
    SNF_VBP_agg             = "^FY 2025 SNF VBP Aggregate Performance$",
    SNF_VBP_fac             = "^FY 2025 SNF VBP Facility-Level Dataset$",
    SNF_quality_nation      = "^Skilled Nursing Facility Quality Reporting Program - National Data$",
    SNF_quality_provider    = "^Skilled Nursing Facility Quality Reporting Program - Provider Data$",
    SNF_quality_swing       = "^Skilled Nursing Facility Quality Reporting Program - Swing Beds - Provider Data$",
    NH_ownership            = "^Ownership$",
    NH_penalties            = "^Penalties$",
    NH_provider             = "^Provider Information$",
    NH_citation             = "^Citation Code Look-up$",
    NH_fire                 = "^Fire Safety Deficiencies$",
    NH_deficiencies         = "^Health Deficiencies$",
    NH_inspection           = "^Inspection Dates$",
    NH_quality_mds          = "^MDS Quality Measures$",
    NH_quality_claims       = "^Medicare Claims Quality Measures$",
    NH_state_avg            = "^State US Averages$",
    NH_state_cut            = "^State-Level Health Inspection Cut Points$",
    NH_interval             = "^Nursing Home Data Collection Interval",
    DIAL_facility           = "^Dialysis Facility - Listing by Facility$",
    DIAL_national           = "^Dialysis Facility - National Averages$",
    DIAL_state              = "^Dialysis Facility - State Averages$",
    ESRD_depression         = "^ESRD QIP - Clinical Depression Screening and Follow-up",
    ESRD_complete           = "^ESRD QIP - Complete QIP Data",
    ESRD_adequacy           = "^ESRD QIP - Dialysis Adequacy",
    ESRD_footnotes          = "^ESRD QIP - Footnotes",
    ESRD_hypercalcemia      = "^ESRD QIP - Hypercalcemia",
    ESRD_medication         = "^ESRD QIP - Medication Reconciliation",
    ESRD_infection          = "^ESRD QIP - NHSN Bloodstream Infection",
    ESRD_event              = "^ESRD QIP - NHSN Dialysis Event Measure",
    ESRD_waitlist           = "^ESRD QIP - Percentage of Prevalent Patients Waitlisted",
    ESRD_hospitalization    = "^ESRD QIP - Standardized Hospitalization Ratio",
    ESRD_readmission        = "^ESRD QIP - Standardized Readmission Ratio",
    ESRD_transfusion        = "^ESRD QIP - Standardized Transfusion Ratio",
    ESRD_performance        = "^ESRD QIP - Total Performance Scores",
    ESRD_ultrafiltration    = "^ESRD QIP - Ultrafiltration Rate",
    ESRD_vascular           = "^ESRD QIP - Vascular Access Topic",
    CAHPS_SPICE_nation    = "^Hospice care - National CAHPS Hospice Survey Data$",
    CAHPS_SPICE_provider  = "^Hospice care - Provider CAHPS Hospice Survey Data$",
    CAHPS_SPICE_state     = "^Hospice care - State CAHPS Hospice Survey Data$",
    CAHPS_HHC_patient         = "^Home Health Care - Patient Survey \\(HHCAHPS\\) 2023Q4 to 2024Q3$",
    CAHPS_HHC_measure         = "^Home Health Care - Patient Survey \\(HHCAHPS\\) Measure Dates 2023Q4 to 2024Q3$",
    CAHPS_HHC_national        = "^Home Health Care - Patient Survey \\(HHCAHPS\\) National Data 2023Q4 to 2024Q3$",
    CAHPS_HHC_state           = "^Home Health Care - Patient Survey \\(HHCAHPS\\) State Data 2023Q4 to 2024Q3$",
    CAHPS_ICH_esrd          = "^ESRD QIP - In-Center Hemodialysis Consumer Assessment Of Healthcare Providers And Services Systems (ICH CAHPS) Survey",
    CAHPS_ICH_facility      = "^Patient survey \\(ICH CAHPS\\) - Facility$",
    CAHPS_ICH_national      = "^Patient survey \\(ICH CAHPS\\) - National$",
    CAHPS_ICH_state         = "^Patient survey \\(ICH CAHPS\\) - State$",




    HVBP_outcomes = "Hospital Value-Based Purchasing \\(HVBP\\) - Clinical Outcomes Domain Scores",
    HVBP_efficiency = "Hospital Value-Based Purchasing \\(HVBP\\) - Efficiency Scores",
    HVBP_engagement = "Hospital Value-Based Purchasing \\(HVBP\\) - Person and Community Engagement Domain Scores \\(HCAHPS\\)",
    HVBP_safety = "Hospital Value-Based Purchasing \\(HVBP\\) - Safety",
    HVBP_performance = "Hospital Value-Based Purchasing \\(HVBP\\) - Total Performance Score",
    HAC_reduction = "Hospital-Acquired Condition \\(HAC\\) Reduction Program",
    HRR_reduction = "Hospital Readmissions Reduction Program",
    IPF_national = "Inpatient Psychiatric Facility Quality Measure Data - National",
    IPF_facility = "Inpatient Psychiatric Facility Quality Measure Data - by Facility",
    IPF_state = "Inpatient Psychiatric Facility Quality Measure Data - by State",
    MSPB_claim = "Medicare Hospital Spending by Claim",
    MSPB_hospital = "Medicare Spending Per Beneficiary - Hospital",
    MSPB_decimal = "Medicare Spending Per Beneficiary - Hospital Additional Decimal Places",
    MSPB_national = "Medicare Spending Per Beneficiary - National",
    MSPB_state = "Medicare Spending Per Beneficiary - State",
    OUT_img_hospital = "Outpatient Imaging Efficiency - Hospital",
    OUT_img_national = "Outpatient Imaging Efficiency - National",
    OUT_img_state = "Outpatient Imaging Efficiency - State",
    CAHPS_OAS_footnotes = "Outpatient and Ambulatory Surgery Consumer Assessment of Healthcare Providers and Systems \\(OAS CAHPS\\) survey - Footnotes",
    CAHPS_OAS_ASC_facility = "Outpatient and Ambulatory Surgery Consumer Assessment of Healthcare Providers and Systems \\(OAS CAHPS\\) survey for ambulatory surgical centers - Facility",
    CAHPS_OAS_ASC_national = "Outpatient and Ambulatory Surgery Consumer Assessment of Healthcare Providers and Systems \\(OAS CAHPS\\) survey for ambulatory surgical centers - National",
    CAHPS_OAS_ASC_state = "Outpatient and Ambulatory Surgery Consumer Assessment of Healthcare Providers and Systems \\(OAS CAHPS\\) survey for ambulatory surgical centers - State",
    CAHPS_OAS_HOSP_facility = "Outpatient and Ambulatory Surgery Consumer Assessment of Healthcare Providers and Systems \\(OAS CAHPS\\) survey for hospital outpatient departments - Facility",
    CAHPS_OAS_HOSP_national = "Outpatient and Ambulatory Surgery Consumer Assessment of Healthcare Providers and Systems \\(OAS CAHPS\\) survey for hospital outpatient departments - National",
    CAHPS_OAS_HOSP_state = "Outpatient and Ambulatory Surgery Consumer Assessment of Healthcare Providers and Systems \\(OAS CAHPS\\) survey for hospital outpatient departments - State",
    PALL_Hospital = "Palliative Care - PPS-Exempt Cancer Hospital - Hospital",
    PALL_National = "Palliative Care - PPS-Exempt Cancer Hospital - National",
    HCAHPS_PCH_Hospital = "Patient Survey \\(PCH - HCAHPS\\) PPS-Exempt Cancer Hospital - Hospital",
    HCAHPS_PCH_National = "Patient Survey \\(PCH - HCAHPS\\) PPS-Exempt Cancer Hospital - National",
    HCAHPS_PCH_State = "Patient Survey \\(PCH - HCAHPS\\) PPS-Exempt Cancer Hospital - State",
    HCAHPS_Hospital = "Patient survey \\(HCAHPS\\) - Hospital",
    HCAHPS_National = "Patient survey \\(HCAHPS\\) - National",
    HCAHPS_State = "Patient survey \\(HCAHPS\\) - State",
    Timely_Hospital = "Timely and Effective Care - Hospital",
    Timely_National = "Timely and Effective Care - National",
    Timely_State = "Timely and Effective Care - State",
    Unplanned_Hospital = "Unplanned Hospital Visits - Hospital",
    Unplanned_National = "Unplanned Hospital Visits - National",
    Unplanned_State = "Unplanned Hospital Visits - State",
    VA_Behavioral = "Veterans Health Administration Behavioral Health Data",
    VA_Provider = "Veterans Health Administration Provider Level Data",
    VA_Timely = "Veterans Health Administration Timely and Effective Care Data",

    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )

  if (!exists("catalog")) .catalog <- catalogs()

  res <- select_alias(.catalog$pro$main, x)

  if (empty(res)) cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)

  c(res)

}

#' @autoglobal
#' @noRd
select_pro_group <- function(x, call = caller_env()) {
  switch(
    x,
    MIPS = list(
      group = "PY 2022 MIPS Public Reporting",
      alias = c(
        "MIPS_performance",
        "MIPS_patient",
        "MIPS_clinician",
        "MIPS_group",
        "MIPS_virtual"
        )
      ),
    PDC = list(
      group = "Provider Data Catalog",
      alias = c(
        "PDC_affiliations",
        "PDC_clinicians",
        "PDC_utilization"
        )
      ),
    LTCH = list(
      group = "Long-Term Care Hospitals",
      alias = c(
        "LTCH_general",
        "LTCH_provider",
        "LTCH_national"
      )
    ),
    IRF = list(
      group = "Inpatient Rehabilitation Facilities",
      alias = c(
        "IRF_conditions",
        "IRF_general",
        "IRF_provider",
        "IRF_national"
      )
    ),
    SPICE = list(
      group = "Hospices",
      alias = c(
        "SPICE_general",
        "SPICE_provider",
        "SPICE_state",
        "SPICE_zip",
        "SPICE_national"
      )
    ),
    CAHPS_hospice = list(
      group = "CAHPS Hospice Survey Data",
      alias = c(
        "CAHPS_SPICE_nation",
        "CAHPS_SPICE_provider",
        "CAHPS_SPICE_state"
      )
    ),
    HHVBP = list(
      group = "Expanded Home Health Value-Based Purchasing (HHVBP) Model",
      alias = c(
        "HHVBP_agency",
        "HHVBP_cohort"
      )
    ),
    HHC = list(
      group = "Home Health Care Agencies",
      alias = c(
        "HHC_range",
        "HHC_national",
        "HHC_state",
        "HHC_zip",
        "HHC_agency"
      )
    ),
    HHCAHPS = list(
      group = "Home Health Care Patient Survey Data (HHCAHPS)",
      alias = c(
        "CAHPS_HHC_patient",
        "CAHPS_HHC_measure",
        "CAHPS_HHC_national",
        "CAHPS_HHC_state"
      )
    ),
    SNF_VBP = list(
      group = "FY 2025 SNF VBP",
      alias = c(
        "SNF_VBP_agg",
        "SNF_VBP_fac"
      )
    ),
    SNF_quality = list(
      group = "SNF Quality Measures",
      alias = c(
        "SNF_quality_nation",
        "SNF_quality_provider",
        "SNF_quality_swing"
      )
    ),
    NH_pro = list(
      group = "Nursing Homes",
      alias = c(
        "NH_ownership",
        "NH_penalties",
        "NH_provider",
        "NH_citation",
        "NH_fire",
        "NH_deficiencies",
        "NH_inspection",
        "NH_quality_mds",
        "NH_quality_claims",
        "NH_state_avg",
        "NH_state_cut",
        "NH_interval"
      )
    ),
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
}
