#' @autoglobal
#' @noRd
pro_main <- function(x, call = caller_env()) {
  x <- switch(
    x,
    suppliers               = "^Medical Equipment Suppliers$",
    PDC_affiliations        = "^Facility Affiliation Data$",
    PDC_clinicians          = "^National Downloadable File$",
    PDC_utilization         = "^Utilization Data$",
    MIPS_perform            = "^PY 2022 Clinician Public Reporting: Overall MIPS Performance$",
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
    CAHPS_hospice_nation    = "^Hospice care - National CAHPS Hospice Survey Data$",
    CAHPS_hospice_provider  = "^Hospice care - Provider CAHPS Hospice Survey Data$",
    CAHPS_hospice_state     = "^Hospice care - State CAHPS Hospice Survey Data$",
    HHVBP_agency            = "^Expanded Home Health Value-Based Purchasing \\(HHVBP\\) Model - Agency Data$",
    HHVBP_cohort            = "^Expanded Home Health Value-Based Purchasing \\(HHVBP\\) Model - Cohort Data$",
    HHC_range               = "^Home Health Care - Measure Date Range$",
    HHC_national            = "^Home Health Care - National Data$",
    HHC_state               = "^Home Health Care - State by State Data$",
    HHC_zip                 = "^Home Health Care - Zip Codes$",
    HHC_agency              = "^Home Health Care Agencies$",
    HHCAHPS_patient         = "^Home Health Care - Patient Survey \\(HHCAHPS\\) 2023Q4 to 2024Q3$",
    HHCAHPS_measure         = "^Home Health Care - Patient Survey \\(HHCAHPS\\) Measure Dates 2023Q4 to 2024Q3$",
    HHCAHPS_national        = "^Home Health Care - Patient Survey \\(HHCAHPS\\) National Data 2023Q4 to 2024Q3$",
    HHCAHPS_state           = "^Home Health Care - Patient Survey \\(HHCAHPS\\) State Data 2023Q4 to 2024Q3$",
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
    NH_interval             = "^Nursing Home Data Collection Interval$",
    PRO_dialysis_fac        = "^Dialysis Facility - Listing by Facility$",
    PRO_dialysis_nat        = "^Dialysis Facility - National Averages$",
    PRO_dialysis_state      = "^Dialysis Facility - State Averages$",
    ESRD_depression         = "^ESRD QIP - Clinical Depression Screening and Follow-up",
    ESRD_complete           = "^ESRD QIP - Complete QIP Data",
    ESRD_adequacy           = "^ESRD QIP - Dialysis Adequacy",
    ESRD_footnotes          = "^ESRD QIP - Footnotes",
    ESRD_hypercalcemia      = "^ESRD QIP - Hypercalcemia",
    ESRD_CAHPS              = "^ESRD QIP - In-Center Hemodialysis Consumer Assessment Of Healthcare Providers And Services Systems (ICH CAHPS) Survey",
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
    ICH_CAHPS_fac           = "^Patient survey \\(ICH CAHPS\\) - Facility$",
    ICH_CAHPS_nat           = "^Patient survey \\(ICH CAHPS\\) - National$",
    ICH_CAHPS_state         = "^Patient survey \\(ICH CAHPS\\) - State$",
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )

  if (!exists("catalog")) .catalog <- catalogs()

  res <- select_alias(.catalog$pro, x)

  if (empty(res)) cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)

  c(res)

}

#' @autoglobal
#' @noRd
pro_group <- function(x, call = caller_env()) {
  switch(
    x,
    MIPS = list(
      group = "PY 2022 MIPS Public Reporting",
      alias = c(
        "MIPS_perform",
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
        "CAHPS_hospice_nation",
        "CAHPS_hospice_provider",
        "CAHPS_hospice_state"
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
        "HHCAHPS_patient",
        "HHCAHPS_measure",
        "HHCAHPS_national",
        "HHCAHPS_state"
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
