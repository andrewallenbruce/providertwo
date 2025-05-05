#' @autoglobal
#' @noRd
select_caid <- function(x, call = caller_env()) {
  x <- switch(
    x,
    ACA_FUL             = "ACA Federal Upper Limits",
    MLR_summary         = "MLR Summary Reports",
    MLTSS_enroll        = "Managed Long Term Services and Supports \\(MLTSS\\) Enrollees",
    MCD_enterprise      = "Medicaid Enterprise System Datatable",
    DSH_payments        = "Disproportionate Share Hospital \\(DSH\\) Payments - Annual Reporting Requirements",
    DPR_index           = "Division of Pharmacy Releases Index dataset",
    MDRP_new            = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program",
    CAHPS_NAM           = "NAM CAHPS 2014 Public Use",
    enroll_mon_test     = "Monthly Enrollment - Test",
    enroll_new_adult    = "Medicaid Enrollment - New Adult Group",
    pi_data             = "PI dataset",
    tiles               = "category_tiles",

    NADAC               = "^NADAC$",
    NADAC_first         = "^First Time NADAC Rates$",
    NADAC_compare       = "^NADAC Comparison$",

    DEMO_well           = "^Medicaid and CHIP enrollees who received a well",
    DEMO_sud            = "^Medicaid and CHIP enrollees who received mental health or SUD services$",
    DEMO_disability     = "^Medicaid enrollees who qualify for benefits based on disability$",
    DEMO_premature      = "^Prematurity and severe maternal morbidity among Medicaid",
    DEMO_language       = "^Primary language spoken by the Medicaid and CHIP population$",
    DEMO_race           = "^Race and ethnicity of the national Medicaid and CHIP population$",
    DEMO_rural          = "^Rural Medicaid and CHIP enrollees$",
    DEMO_waive          = "^Section 1915",

    SERV_acute          = "^Acute Care Services Provided to the Medicaid and CHIP Population$",
    SERV_behavior       = "^Behavioral Health ServicesProvided to the Medicaid and CHIP Population$",
    SERV_perinatal      = "^Perinatal Care Services Provided to Medicaid and CHIP Beneficiaries ages 15 to 44",
    SERV_screen         = "^Health Screenings Provided to Medicaid and CHIP Beneficiaries Under Age 19",
    SERV_contra         = "^Contraceptive Care Services Provided to Medicaid and CHIP Beneficiaries ages 15 to 44",
    SERV_dental         = "^Dental Services Provided to Medicaid and CHIP Beneficiaries Under Age 19",
    SERV_pregnancy      = "^Pregnancy Outcomes for Medicaid and CHIP Beneficiaries ages 15 to 44",
    SERV_telehealth     = "^Telehealth Services Provided to the Medicaid and CHIP Population",
    SERV_vaccination    = "^Vaccinations Provided to the Medicaid and CHIP Population under age 19",
    SERV_lead           = "^Blood Lead Screening Services Provided to Medicaid and CHIP Beneficiaries",
    SERV_respiratory    = "^Respiratory Conditions in the Medicaid and CHIP Population",

    BENE_behavior       = "Beneficiaries receiving a behavioral health service by behavioral health condition, 2017-2021",
    BENE_physical       = "Beneficiaries receiving a physical health service among beneficiaries receiving a SUD service by physical health cond, 2017-2021",
    BENE_mental         = "Beneficiaries receiving a physical hlth serv among beneficiaries receiving a mental hlth serv, by physical hlth cond, 2017-2021",
    BENE_integrated     = "Beneficiaries who could benefit from integrated care, 2017-2021",
    BENE_nas            = "Number and rate of NAS per 1,000 births in newborns whose deliveries were covered by Medicaid or CHIP, 2017 - 2021",
    BENE_smm            = "Number and rate of SMM among Medicaid- and CHIP-covered deliveries, 2017 - 2021",
    BENE_postpart       = "Number of pregnant and postpartum Medicaid and CHIP beneficiaries, 2017-2021",
    BENE_delivery       = "Rate of NAS per 1,000 births in newborns whose deliveries were covered by Medicaid or CHIP, 2017 - 2019",

    PKG_month           = "Benefit Package for Medicaid and CHIP Beneficiaries by Month",
    PKG_year            = "Benefit Package for Medicaid and CHIP Beneficiaries by Year",

    DRUG_amp_mon        = "Drug AMP Reporting - Monthly",
    DRUG_amp_qtr        = "Drug AMP Reporting - Quarterly",
    DRUG_products       = "Drug Products in the Medicaid Drug Rebate Program",
    DRUG_clot           = "^Clotting Factor Drug Report",
    DRUG_pediatric      = "Exclusive Pediatric Drugs",
    DRUG_contact_manu   = "Drug Manufacturer Contacts",
    DRUG_contact_state  = "Medicaid Drug Rebate Program State Contact Information",

    DUAL_month          = "Dual Status Information for Medicaid and CHIP Beneficiaries by Month",
    DUAL_year           = "Dual Status Information for Medicaid and CHIP Beneficiaries by Year",

    MEG_month           = "Major Eligibility Group Information for Medicaid and CHIP Beneficiaries by Month",
    MEG_year            = "Major Eligibility Group Information for Medicaid and CHIP Beneficiaries by Year",

    CMS64_CAA           = "Medicaid CMS-64 CAA 2023 Increased FMAP Expenditure Data Collected through MBES/CBES",
    CMS64_FFCRA         = "Medicaid CMS-64 FFCRA Increased FMAP Expenditure",
    CMS64_adult         = "Medicaid CMS-64 New Adult Group Expenditures",

    MC_summary          = "Managed Care Enrollment Summary",
    MC_program_state    = "^Managed Care Programs\\sby\\sState$",
    MC_program_plan     = "Managed Care Enrollment by Program and Plan",
    MC_program_pop_all  = "Managed Care Enrollment by Program and Population \\(All\\)",
    MC_program_pop_dual = "Managed Care Enrollment by Program and Population \\(Duals\\)",
    MC_feat_pop         = "Managed Care Features By Enrollment Population",
    MC_feat_qa          = "Managed Care Features by QA and Performance Incentive",
    MC_bene_month       = "Managed Care Information for Medicaid and CHIP Beneficiaries by Month",
    MC_bene_year        = "Managed Care Information for Medicaid and CHIP Beneficiaries by Year",
    MC_share            = "Share of Medicaid Enrollees in Managed Care",

    FIN_mgmt            = "Medicaid Financial Management Data$",
    FIN_nation          = "Medicaid Financial Management Data National Totals",

    Unwind_marketplace     = "HealthCare\\.gov Marketplace Medicaid Unwinding Report",
    Unwind_transition      = "HealthCare\\.gov Transitions Marketplace Medicaid Unwinding Report",
    Unwind_historic        = "Separate CHIP Enrollment by Month and State\\sHistoric CAA/Unwinding Period",
    Unwind_SBM             = "State-based Marketplace \\(SBM\\) Medicaid Unwinding Report",

    CHIP_bene_month        = "Program Information for Medicaid and CHIP Beneficiaries by Month",
    CHIP_bene_year         = "Program Information for Medicaid and CHIP Beneficiaries by Year",
    CHIP_application       = "^CHIP Applications, Eligibility Determinations, and Enrollment Data",
    CHIP_continue          = "^Continuous Eligibility for Medicaid and CHIP Coverage$",
    CHIP_express           = "Express Lane Eligibility for Medicaid and CHIP Coverage",
    CHIP_metrics           = "Medicaid and CHIP CAA Reporting Metrics",
    CHIP_levels            = "Medicaid and CHIP Eligibility Levels",
    CHIP_renewal           = "Medicaid and CHIP Updated Renewal Outcomes",
    CHIP_presume           = "Presumptive Eligibility for Medicaid and CHIP Coverage",
    CHIP_separate          = "Separate CHIP Enrollment by Month and State",
    CHIP_application_state = "State Medicaid and CHIP Applications, Eligibility Determinations, and Enrollment Data",
    CHIP_processing_state  = "State Medicaid and CHIP Eligibility Processing Data",
    CHIP_test              = "State Medicaid and CHIP Test",

    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )

  if (!exists("catalog")) .catalog <- catalogs()

  res <- select_alias(.catalog$caid$main, x)

  # select_alias(
  #   .catalog$caid$main,
  #   "^Managed Care Programs\\sby\\sState$"
  #   ) |>
  #   _$title

  if (empty(res))     cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  if (nrow(res) > 1L) cli_abort(c("x" = "> 1 match found for {.val {x}}."), call = call)

  c(res)

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
    NADAC = list(
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
select_caid_temp <- function(x, call = caller_env()) {

  x <- switch(
    x,
    NADAC  = "NADAC",
    MCP    = "^Managed Care Programs",
    rebate = "^Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program",
    blood  = "^Pricing Comparison for Blood Disorder Treatments",
    drug   = "^State Drug Utilization Data",
    HCQ    = "Child and Adult Health Care Quality Measures",
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )

  if (!exists("catalog")) .catalog <- catalogs()

  res <- select_alias(.catalog$caid$temp, x)

  if (empty(res)) cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)

  list(
    title       = res$title[1],
    description = res$description[1],
    modified    = res$modified[1],
    periodicity = res$periodicity[1],
    identifier  = res$identifier[1],
    endpoints   = slt(res, year, identifier, download, dictionary)
  )
}
