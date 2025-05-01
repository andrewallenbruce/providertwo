#' @autoglobal
#' @noRd
select_caid <- function(x, call = caller_env()) {
  x <- switch(
    x,
    ACA_FUL             = "ACA Federal Upper Limits",
    MLR                 = "MLR Summary Reports",
    ELE                 = "Express Lane Eligibility for Medicaid and CHIP Coverage",
    MLTSS               = "Managed Long Term Services and Supports \\(MLTSS\\) Enrollees",
    enterprise          = "Medicaid Enterprise System Datatable",
    CHIP_data           = "^CHIP Applications, Eligibility Determinations, and Enrollment Data",
    CHIP_elig           = "^Continuous Eligibility for Medicaid and CHIP Coverage$",
    DSH                 = "Disproportionate Share Hospital \\(DSH\\) Payments - Annual Reporting Requirements",
    DPRI                = "Division of Pharmacy Releases Index dataset",
    DRP_new             = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program",

    NADAC_first         = "^First Time NADAC Rates$",
    NADAC               = "^NADAC$",
    NADAC_compare       = "^NADAC Comparison$",

    demo_well           = "^Medicaid and CHIP enrollees who received a well",
    demo_lead           = "^Blood Lead Screening Services Provided to Medicaid and CHIP Beneficiaries",
    demo_sud            = "^Medicaid and CHIP enrollees who received mental health or SUD services$",
    demo_disability     = "^Medicaid enrollees who qualify for benefits based on disability$",
    demo_premature      = "^Prematurity and severe maternal morbidity among Medicaid",
    demo_language       = "^Primary language spoken by the Medicaid and CHIP population$",
    demo_race           = "^Race and ethnicity of the national Medicaid and CHIP population$",
    demo_rural          = "^Rural Medicaid and CHIP enrollees$",
    demo_waive          = "^Section 1915",
    demo_respiratory    = "^Respiratory Conditions in the Medicaid and CHIP Population",

    service_acute       = "^Acute Care Services Provided to the Medicaid and CHIP Population$",
    service_behavior    = "^Behavioral Health ServicesProvided to the Medicaid and CHIP Population$",
    service_perinatal   = "^Perinatal Care Services Provided to Medicaid and CHIP Beneficiaries ages 15 to 44",
    service_screen      = "^Health Screenings Provided to Medicaid and CHIP Beneficiaries Under Age 19",
    service_contra      = "^Contraceptive Care Services Provided to Medicaid and CHIP Beneficiaries ages 15 to 44",
    service_dental      = "^Dental Services Provided to Medicaid and CHIP Beneficiaries Under Age 19",
    service_pregnancy   = "^Pregnancy Outcomes for Medicaid and CHIP Beneficiaries ages 15 to 44",
    service_telehealth  = "Telehealth Services Provided to the Medicaid and CHIP Population",
    service_vaccination = "Vaccinations Provided to the Medicaid and CHIP Population under age 19",

    BENE_behavior       = "Beneficiaries receiving a behavioral health service by behavioral health condition, 2017-2021",
    BENE_physical       = "Beneficiaries receiving a physical health service among beneficiaries receiving a SUD service by physical health cond, 2017-2021",
    BENE_mental         = "Beneficiaries receiving a physical hlth serv among beneficiaries receiving a mental hlth serv, by physical hlth cond, 2017-2021",
    BENE_integrated     = "Beneficiaries who could benefit from integrated care, 2017-2021",
    BENE_NAS            = "Number and rate of NAS per 1,000 births in newborns whose deliveries were covered by Medicaid or CHIP, 2017 - 2021",
    BENE_SMM            = "Number and rate of SMM among Medicaid- and CHIP-covered deliveries, 2017 - 2021",
    BENE_POSTPART       = "Number of pregnant and postpartum Medicaid and CHIP beneficiaries, 2017-2021",
    BENE_delivery       = "Rate of NAS per 1,000 births in newborns whose deliveries were covered by Medicaid or CHIP, 2017 - 2019",

    BENE_pkg_month      = "Benefit Package for Medicaid and CHIP Beneficiaries by Month",
    BENE_pkg_year       = "Benefit Package for Medicaid and CHIP Beneficiaries by Year",

    DRUG_amp_month      = "Drug AMP Reporting - Monthly",
    DRUG_amp_quarter    = "Drug AMP Reporting - Quarterly",
    DRUG_contracts      = "Drug Manufacturer Contacts",
    DRUG_products       = "Drug Products in the Medicaid Drug Rebate Program",
    DRUG_clot           = "^Clotting Factor Drug Report",
    DRUG_pediatric      = "Exclusive Pediatric Drugs",
    DRUG_contact        = "Medicaid Drug Rebate Program State Contact Information",

    DUAL_month          = "Dual Status Information for Medicaid and CHIP Beneficiaries by Month",
    DUAL_year           = "Dual Status Information for Medicaid and CHIP Beneficiaries by Year",

    MEGI_month          = "Major Eligibility Group Information for Medicaid and CHIP Beneficiaries by Month",
    MEGI_year           = "Major Eligibility Group Information for Medicaid and CHIP Beneficiaries by Year",

    CAHPS_NAM           = "NAM CAHPS 2014 Public Use",

    CMS64_CAA           = "Medicaid CMS-64 CAA 2023 Increased FMAP Expenditure Data Collected through MBES/CBES",
    CMS64_FFCRA         = "Medicaid CMS-64 FFCRA Increased FMAP Expenditure",
    CMS64_adult         = "Medicaid CMS-64 New Adult Group Expenditures",

    MC_summary          = "Managed Care Enrollment Summary",
    MC_program_state    = "Managed Care Programs by State",
    MC_program_plan     = "Managed Care Enrollment by Program and Plan",
    MC_program_pop_all  = "Managed Care Enrollment by Program and Population \\(All\\)",
    MC_program_pop_dual = "Managed Care Enrollment by Program and Population \\(Duals\\)",
    MC_feat_pop         = "Managed Care Features By Enrollment Population",
    MC_feat_qa          = "Managed Care Features by QA and Performance Incentive",
    MC_bene_month       = "Managed Care Information for Medicaid and CHIP Beneficiaries by Month",
    MC_bene_year        = "Managed Care Information for Medicaid and CHIP Beneficiaries by Year",

    FIN_mgmt            = "Medicaid Financial Management Data$",
    FIN_nation          = "Medicaid Financial Management Data  National Totals",

    # demo_ = "HealthCare\\.gov Marketplace Medicaid Unwinding Report",
    # demo_ = "HealthCare\\.gov Transitions Marketplace Medicaid Unwinding Report",

    # demo_ = "Medicaid Enrollment - New Adult Group",

    # demo_ = "Medicaid and CHIP CAA Reporting Metrics",
    # demo_ = "Medicaid and CHIP Eligibility Levels",
    # demo_ = "Medicaid and CHIP Updated Renewal Outcomes",

    # demo_ = "Presumptive Eligibility for Medicaid and CHIP Coverage",
    # demo_ = "Program Information for Medicaid and CHIP Beneficiaries by Month",
    # demo_ = "Program Information for Medicaid and CHIP Beneficiaries by Year",

    # demo_ = "Separate CHIP Enrollment by Month and State",
    # demo_ = "Separate CHIP Enrollment by Month and State\\s.+\\sHistoric CAA/Unwinding Period",
    # demo_ = "Share of Medicaid Enrollees in Managed Care",
    # demo_ = "State Medicaid and CHIP Applications, Eligibility Determinations, and Enrollment Data",
    # demo_ = "State Medicaid and CHIP Eligibility Processing Data",
    # demo_ = "State Medicaid and CHIP Test",
    # demo_ = "State-based Marketplace \\(SBM\\) Medicaid Unwinding Report",

    # demo_ = "Monthly Enrollment - Test",
    # demo_ = "PI dataset",
    # demo_ = "category_tiles",

    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )

  if (!exists("catalog")) .catalog <- catalogs()

  res <- select_alias(.catalog$caid$main, x)

  # select_alias(
  #   .catalog$caid$main,
  #   "^Behavioral"
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
        "demo_well",
        "demo_lead",
        "demo_sud",
        "demo_disability",
        "demo_premature",
        "demo_language",
        "demo_race",
        "demo_rural",
        "demo_waive",
        "demo_respiratory"
        )
    ),
    services = list(
      group = "Services Provided to the Medicaid and CHIP Population",
      alias = c(
        "service_acute",
        "service_behavior",
        "service_perinatal",
        "service_screen",
        "service_contra",
        "service_dental",
        "service_pregnancy",
        "service_telehealth",
        "service_vaccination"
        )
    ),
    beneficiaries = list(
      group = "Beneficiaries Receiving A Service",
      alias = c(
        "BENE_behavior",
        "BENE_physical",
        "BENE_mental",
        "BENE_integrated",
        "BENE_NAS",
        "BENE_SMM",
        "BENE_POSTPART",
        "BENE_delivery"
      )
    ),
    dual = list(
      group = "Dual Status Information for Medicaid and CHIP Beneficiaries",
      alias = c(
        "DUAL_month",
        "DUAL_year"
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
    MCP    = "Managed Care Programs By State",
    rebate = "^Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program",
    blood  = "^Pricing Comparison for Blood Disorder Treatments",
    drug   = "^State Drug Utilization Data",
    HCQ    = "Child and Adult Health Care Quality Measures",
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call))

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

#' @autoglobal
#' @noRd
select_caid_troup <- function(x, call = caller_env()) {
  switch(
    x,
    NAME   = list(
      group = "GROUPNAME",
      alias = c(
        "ALIAS_NAME"
      )
    ),
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
}
