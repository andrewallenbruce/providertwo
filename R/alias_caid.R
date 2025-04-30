#' @autoglobal
#' @noRd
select_caid <- function(x, call = caller_env()) {
  x <- switch(
    x,
    ACA_lim         = "ACA Federal Upper Limits",
    MLR             = "MLR Summary Reports",
    enterprise      = "Medicaid Enterprise System Datatable",
    blood_lead      = "^Blood Lead Screening Services Provided to Medicaid and CHIP Beneficiaries",
    blood_clot      = "^Clotting Factor Drug Report",
    chip_data       = "^CHIP Applications, Eligibility Determinations, and Enrollment Data",
    chip_elig       = "^Continuous Eligibility for Medicaid and CHIP Coverage$",

    demo_well       = "^Medicaid and CHIP enrollees who received a well",
    demo_sud        = "^Medicaid and CHIP enrollees who received mental health or SUD services$",
    demo_disable    = "^Medicaid enrollees who qualify for benefits based on disability$",
    demo_premature  = "^Prematurity and severe maternal morbidity among Medicaid",
    demo_language   = "^Primary language spoken by the Medicaid and CHIP population$",
    demo_race       = "^Race and ethnicity of the national Medicaid and CHIP population$",
    demo_rural      = "^Rural Medicaid and CHIP enrollees$",
    demo_waive      = "^Section 1915",
    demo_acute      = "^Acute Care Services Provided to the Medicaid and CHIP Population$",
    demo_behavior   = "^Behavioral\\sHealth\\sServices\\sProvided\\sto\\sthe\\sMedicaid\\sand\\sCHIP\\sPopulation",

    BENE_behavior   = "Beneficiaries receiving a behavioral health service by behavioral health condition, 2017-2021",
    BENE_physical   = "Beneficiaries receiving a physical health service among beneficiaries receiving a SUD service by physical health cond, 2017-2021",
    BENE_mental     = "Beneficiaries receiving a physical hlth serv among beneficiaries receiving a mental hlth serv, by physical hlth cond, 2017-2021",
    BENE_integrated = "Beneficiaries who could benefit from integrated care, 2017-2021",
    BENE_pkg_month  = "Benefit Package for Medicaid and CHIP Beneficiaries by Month",
    BENE_pkg_year   = "Benefit Package for Medicaid and CHIP Beneficiaries by Year",

    chip_contra = "Contraceptive Care Services Provided to Medicaid and CHIP Beneficiaries ages 15 to 44",
    chip_dental = "Dental Services Provided to Medicaid and CHIP Beneficiaries Under Age 19",
    DSH         = "Disproportionate Share Hospital \\(DSH\\) Payments - Annual Reporting Requirements",
    DPRI        = "Division of Pharmacy Releases Index dataset",

    drug_amp_month = "Drug AMP Reporting - Monthly",
    drug_amp_quarter = "Drug AMP Reporting - Quarterly",

    drug_contracts = "Drug Manufacturer Contacts",
    drug_products = "Drug Products in the Medicaid Drug Rebate Program",

    dual_month = "Dual Status Information for Medicaid and CHIP Beneficiaries by Month",
    dual_year = "Dual Status Information for Medicaid and CHIP Beneficiaries by Year",

    drug_ped = "Exclusive Pediatric Drugs",

    ELE = "Express Lane Eligibility for Medicaid and CHIP Coverage",
    # demo_ = "First Time NADAC Rates",
    # demo_ = "Health Screenings Provided to Medicaid and CHIP Beneficiaries Under Age 19",
    # demo_ = "HealthCare.gov Marketplace Medicaid Unwinding Report",
    # demo_ = "HealthCare.gov Transitions Marketplace Medicaid Unwinding Report",
    # demo_ = "Major Eligibility Group Information for Medicaid and CHIP Beneficiaries by Month",
    # demo_ = "Major Eligibility Group Information for Medicaid and CHIP Beneficiaries by Year",
    # demo_ = "Managed Care Enrollment Summary",
    # demo_ = "Managed Care Enrollment by Program and Plan",
    # demo_ = "Managed Care Enrollment by Program and Population (All)",
    # demo_ = "Managed Care Enrollment by Program and Population (Duals)",
    # demo_ = "Managed Care Features By Enrollment Population",
    # demo_ = "Managed Care Features by QA and Performance Incentive",
    # demo_ = "Managed Care Information for Medicaid and CHIP Beneficiaries by Month",
    # demo_ = "Managed Care Information for Medicaid and CHIP Beneficiaries by Year",
    # demo_ = "Managed Care Programs by State",
    # demo_ = "Managed Long Term Services and Supports (MLTSS) Enrollees",
    # demo_ = "Medicaid CMS-64 CAA 2023 Increased FMAP Expenditure Data Collected through MBES/CBES",
    # demo_ = "Medicaid CMS-64 FFCRA Increased FMAP Expenditure",
    # demo_ = "Medicaid CMS-64 New Adult Group Expenditures",
    # demo_ = "Medicaid Drug Rebate Program State Contact Information",
    # demo_ = "Medicaid Enrollment - New Adult Group",
    # demo_ = "Medicaid Financial Management Data",
    # demo_ = "Medicaid Financial Management Data – National Totals",
    # demo_ = "Medicaid and CHIP CAA Reporting Metrics",
    # demo_ = "Medicaid and CHIP Eligibility Levels",
    # demo_ = "Medicaid and CHIP Updated Renewal Outcomes",
    # demo_ = "Medicaid and CHIP enrollees who received a well-child visit",
    # demo_ = "Medicaid and CHIP enrollees who received mental health or SUD services",
    # demo_ = "Medicaid enrollees who qualify for benefits based on disability",
    # demo_ = "Monthly Enrollment - Test",
    # demo_ = "NADAC",
    # demo_ = "NADAC Comparison",
    # demo_ = "NAM CAHPS 2014 Public Use",
    # demo_ = "Number and rate of NAS per 1,000 births in newborns whose deliveries were covered by Medicaid or CHIP, 2017 - 2021",
    # demo_ = "Number and rate of SMM among Medicaid- and CHIP-covered deliveries, 2017 - 2021",
    # demo_ = "Number of pregnant and postpartum Medicaid and CHIP beneficiaries, 2017-2021",
    # demo_ = "PI dataset",
    # demo_ = "Perinatal Care Services Provided to Medicaid and CHIP Beneficiaries ages 15 to 44",
    # demo_ = "Pregnancy Outcomes for Medicaid and CHIP Beneficiaries ages 15 to 44",
    # demo_ = "Prematurity and severe maternal morbidity among Medicaid- and CHIP-covered live births",
    # demo_ = "Presumptive Eligibility for Medicaid and CHIP Coverage",
    # demo_ = "Primary language spoken by the Medicaid and CHIP population",
    # demo_ = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program",
    # demo_ = "Program Information for Medicaid and CHIP Beneficiaries by Month",
    # demo_ = "Program Information for Medicaid and CHIP Beneficiaries by Year",
    # demo_ = "Race and ethnicity of the national Medicaid and CHIP population",
    # demo_ = "Rate of NAS per 1,000 births in newborns whose deliveries were covered by Medicaid or CHIP, 2017 - 2019",
    # demo_ = "Respiratory Conditions in the Medicaid and CHIP Population",
    # demo_ = "Rural Medicaid and CHIP enrollees",
    # demo_ = "SDUD",
    # demo_ = "Section 1915(c) waiver program participants",
    # demo_ = "Separate CHIP Enrollment by Month and State",
    # demo_ = "Separate CHIP Enrollment by Month and State – Historic CAA/Unwinding Period",
    # demo_ = "Share of Medicaid Enrollees in Managed Care",
    # demo_ = "State Medicaid and CHIP Applications, Eligibility Determinations, and Enrollment Data",
    # demo_ = "State Medicaid and CHIP Eligibility Processing Data",
    # demo_ = "State Medicaid and CHIP Test",
    # demo_ = "State-based Marketplace (SBM) Medicaid Unwinding Report",
    # demo_ = "Telehealth Services Provided to the Medicaid and CHIP Population",
    # demo_ = "Vaccinations Provided to the Medicaid and CHIP Population under age 19",
    # demo_ = "category_tiles",

    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )

  if (!exists("catalog")) .catalog <- catalogs()

  res <- select_alias(.catalog$caid$main, x)
  # select_alias(
  #   .catalog$caid$main,
  #   "^Behavioral\\sHealth\\sServices\\sProvided\\sto\\sthe\\sMedicaid\\sand\\sCHIP\\sPopulation"
  #   )

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
        "demo_sud",
        "demo_disable",
        "demo_premature",
        "demo_language",
        "demo_race",
        "demo_rural",
        "demo_waive",
        "demo_acute",
        "demo_behavior"
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

  list_tidy(
    !!!c(slt(res, -data)),
    endpoints   = get_elem(res, "data") |> _[[1]],
    identifier  = endpoints$identifier[1]
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
