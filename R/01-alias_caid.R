#' Medicaid API Endpoints
#' @name medicaid
#' @param alias `<chr>` endpoint or group alias
#' @param call `<env>` environment to use for error reporting
#' @param ... Additional arguments passed to the group constructor
#' @returns An S7 `<class_endpoint>`, `<class_temporal>` or `<class_group>` object
#' @examples
#' caid_endpoint("managed_longterm")
#' caid_temporal("state_drug_util")
#' caid_group("caid_dual_status")
NULL

#' @rdname medicaid
#' @autoglobal
#' @export
caid_endpoint <- function(alias, call = caller_env()) {

  check_required(alias)

  x <- switch(
    alias,
    aca_ful                       = "ACA Federal Upper Limits",
    caid_drug_rebate              = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program",
    caid_enroll_month             = "Monthly Enrollment - Test",
    caid_enroll_adult             = "Medicaid Enrollment - New Adult Group",
    caid_enterprise               = "Medicaid Enterprise System Datatable",
    caid_pi                       = "PI dataset",
    caid_tiles                    = "category_tiles",
    mlr_summary                   = "MLR Summary Reports",
    managed_longterm              = "Managed Long Term Services and Supports \\(MLTSS\\) Enrollees",
    dsh_requirement               = "Disproportionate Share Hospital \\(DSH\\) Payments [-] Annual Reporting Requirements",
    pharm_release_index           = "Division of Pharmacy Releases Index dataset",
    cahps_nam_puf                 = "NAM CAHPS 2014 Public Use",
    caid_nadac                    = "^NADAC$",
    caid_nadac_first              = "^First Time NADAC Rates$",
    caid_nadac_compare            = "^NADAC Comparison$",
    caid_finance_mgmt             = "Medicaid Financial Management Data$",
    caid_finance_nation           = "Medicaid Financial Management Data National Totals",
    caid_chip_bene_month          = "Program Information for Medicaid and CHIP Beneficiaries by Month",
    caid_chip_bene_year           = "Program Information for Medicaid and CHIP Beneficiaries by Year",
    caid_chip_application         = "^CHIP Applications, Eligibility Determinations, and Enrollment Data",
    caid_chip_continue            = "^Continuous Eligibility for Medicaid and CHIP Coverage$",
    caid_chip_express             = "Express Lane Eligibility for Medicaid and CHIP Coverage",
    caid_chip_metrics             = "Medicaid and CHIP CAA Reporting Metrics",
    caid_chip_levels              = "Medicaid and CHIP Eligibility Levels",
    caid_chip_renewal             = "Medicaid and CHIP Updated Renewal Outcomes",
    caid_chip_presume             = "Presumptive Eligibility for Medicaid and CHIP Coverage",
    caid_chip_separate            = "Separate CHIP Enrollment by Month and State",
    caid_chip_application_state   = "State Medicaid and CHIP Applications, Eligibility Determinations, and Enrollment Data",
    caid_chip_processing_state    = "State Medicaid and CHIP Eligibility Processing Data",
    caid_chip_test                = "State Medicaid and CHIP Test",
    demo_wellvisit                = "^Medicaid and CHIP enrollees who received a well",
    demo_mental                   = "^Medicaid and CHIP enrollees who received mental health or SUD services$",
    demo_disability               = "^Medicaid enrollees who qualify for benefits based on disability$",
    demo_prematurity              = "^Prematurity and severe maternal morbidity among Medicaid",
    demo_language                 = "^Primary language spoken by the Medicaid and CHIP population$",
    demo_ethnicity                = "^Race and ethnicity of the national Medicaid and CHIP population$",
    demo_rural                    = "^Rural Medicaid and CHIP enrollees$",
    demo_waiver                   = "^Section 1915",
    service_acute                 = "^Acute Care Services Provided to the Medicaid and CHIP Population$",
    service_behavior              = "^Behavioral Health Services\\s?Provided to the Medicaid and CHIP Population$",
    service_perinatal             = "^Perinatal Care Services Provided to Medicaid and CHIP Beneficiaries ages 15 to 44",
    service_screening             = "^Health Screenings Provided to Medicaid and CHIP Beneficiaries Under Age 19",
    service_contraceptive         = "^Contraceptive Care Services Provided to Medicaid and CHIP Beneficiaries ages 15 to 44",
    service_dental                = "^Dental Services Provided to Medicaid and CHIP Beneficiaries Under Age 19",
    service_pregnancy             = "^Pregnancy Outcomes for Medicaid and CHIP Beneficiaries ages 15 to 44",
    service_telehealth            = "^Telehealth Services Provided to the Medicaid and CHIP Population",
    service_vaccination           = "^Vaccinations Provided to the Medicaid and CHIP Population under age 19",
    service_bloodlead             = "^Blood Lead Screening Services Provided to Medicaid and CHIP Beneficiaries",
    service_respiratory           = "^Respiratory Conditions in the Medicaid and CHIP Population",
    benes_behavior                = "Beneficiaries receiving a behavioral health service by behavioral health condition, 2017-2021",
    benes_physical                = "Beneficiaries receiving a physical health service among beneficiaries receiving a SUD service by physical health cond, 2017-2021",
    benes_mental                  = "Beneficiaries receiving a physical hlth serv among beneficiaries receiving a mental hlth serv, by physical hlth cond, 2017-2021",
    benes_integrated              = "Beneficiaries who could benefit from integrated care, 2017-2021",
    benes_nas                     = "Number and rate of NAS per 1,000 births in newborns whose deliveries were covered by Medicaid or CHIP, 2017 - 2021",
    benes_smm                     = "Number and rate of SMM among Medicaid- and CHIP-covered deliveries, 2017 - 2021",
    benes_pregnant                = "Number of pregnant and postpartum Medicaid and CHIP beneficiaries, 2017-2021",
    benes_delivery                = "Rate of NAS per 1,000 births in newborns whose deliveries were covered by Medicaid or CHIP, 2017 - 2019",
    benefit_pkg_month             = "Benefit Package for Medicaid and CHIP Beneficiaries by Month",
    benefit_pkg_year              = "Benefit Package for Medicaid and CHIP Beneficiaries by Year",
    drug_amp_month                = "Drug AMP Reporting - Monthly",
    drug_amp_quarter              = "Drug AMP Reporting - Quarterly",
    drug_products                 = "Drug Products in the Medicaid Drug Rebate Program",
    drug_clot                     = "^Clotting Factor Drug Report",
    drug_pediatric                = "Exclusive Pediatric Drugs",
    drug_contact_manu             = "Drug Manufacturer Contacts",
    drug_contact_state            = "Medicaid Drug Rebate Program State Contact Information",
    dual_status_month             = "Dual Status Information for Medicaid and CHIP Beneficiaries by Month",
    dual_status_year              = "Dual Status Information for Medicaid and CHIP Beneficiaries by Year",
    meg_month                     = "Major Eligibility Group Information for Medicaid and CHIP Beneficiaries by Month",
    meg_year                      = "Major Eligibility Group Information for Medicaid and CHIP Beneficiaries by Year",
    cms64_caa                     = "Medicaid CMS-64 CAA 2023 Increased FMAP Expenditure Data Collected through MBES/CBES",
    cms64_ffcra                   = "Medicaid CMS-64 FFCRA Increased FMAP Expenditure",
    cms64_adult                   = "Medicaid CMS-64 New Adult Group Expenditures",
    managed_care_summary          = "Managed Care Enrollment Summary",
    managed_care_program_state    = "^Managed Care Programs\\sby\\sState$",
    managed_care_program_plan     = "Managed Care Enrollment by Program and Plan",
    managed_care_program_pop_all  = "Managed Care Enrollment by Program and Population \\(All\\)",
    managed_care_program_pop_dual = "Managed Care Enrollment by Program and Population \\(Duals\\)",
    managed_care_feat_pop         = "Managed Care Features By Enrollment Population",
    managed_care_feat_qa          = "Managed Care Features by QA and Performance Incentive",
    managed_care_bene_month       = "Managed Care Information for Medicaid and CHIP Beneficiaries by Month",
    managed_care_bene_year        = "Managed Care Information for Medicaid and CHIP Beneficiaries by Year",
    managed_care_share            = "Share of Medicaid Enrollees in Managed Care",
    unwind_marketplace            = "HealthCare\\.gov Marketplace Medicaid Unwinding Report",
    unwind_transition             = "HealthCare\\.gov Transitions Marketplace Medicaid Unwinding Report",
    unwind_historic               = "Separate CHIP Enrollment by Month and State\\sHistoric CAA/Unwinding Period",
    unwind_sbm                    = "State-based Marketplace \\(SBM\\) Medicaid Unwinding Report",
    cli_abort(c("x" = "{.emph alias} {.val {alias}} is invalid."), call = call)
  )

  res <- select_alias(the$catalog$caid$end, x)

  if (is_empty(res))  cli_abort(c("x" = "{.val {x}} returned no matches."), call = call)
  if (nrow(res) > 1L) cli_abort(c("x" = "{.val {x}} returned more than 1 match."), call = call)

  x <- c(res)

  class_endpoint(
    identifier  = identifier_(x),
    metadata    = get_metadata(x),
    dimensions  = get_dimensions(x)
  )
}

#' @rdname medicaid
#' @autoglobal
#' @export
caid_temporal <- function(alias, call = caller_env()) {

  check_required(alias)

  x <- switch(
    alias,
    nadac_year            = "^NADAC$",
    managed_care_state    = "^Managed Care Programs by State$",
    caid_drug_rebate_week = "^Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program$",
    blood_disorder        = "^Pricing Comparison for Blood Disorder Treatments$",
    state_drug_util       = "^State Drug Utilization Data$",
    healthcare_quality    = "^Child and Adult Health Care Quality Measures$",
    cli_abort(c("x"       = "{.emph alias} {.val {alias}} is invalid."), call = call)
  )

  res <- select_alias(the$catalog$caid$tmp, x)

  if (is_empty(res)) cli_abort(c("x" = "{.val {x}} returned no matches."), call = call)

  x <- flist(!!!c(slt(res, -endpoints)),
    endpoints   = pluck(get_elem(res, "endpoints"), 1),
    identifier  = endpoints$identifier[1]
  )

  class_temporal(
    metadata    = get_metadata(x),
    dimensions  = get_dimensions(x),
    endpoints   = x$endpoints
  )
}

#' @rdname medicaid
#' @autoglobal
#' @export
caid_group <- function(alias, call = caller_env(), ...) {

  check_required(alias)

  x <- switch(
    alias,
    caid_demographics = list(
      group = "Medicaid and CHIP Enrollee Demographics",
      alias = c(
        "demo_wellvisit",
        "demo_mental",
        "demo_disability",
        "demo_prematurity",
        "demo_language",
        "demo_ethnicity",
        "demo_rural",
        "demo_waiver"
      )
    ),
    caid_services = list(
      group = "Services Provided to the Medicaid and CHIP Population",
      alias = c(
        "service_acute",
        "service_behavior",
        "service_perinatal",
        "service_screening",
        "service_contraceptive",
        "service_dental",
        "service_pregnancy",
        "service_telehealth",
        "service_vaccination",
        "service_bloodlead",
        "service_respiratory"
      )
    ),
    caid_benes = list(
      group = "Beneficiaries Receiving A Service",
      alias = c(
        "benes_behavior",
        "benes_physical",
        "benes_mental",
        "benes_integrated",
        "benes_nas",
        "benes_smm",
        "benes_pregnant",
        "benes_delivery"
      )
    ),
    caid_finance = list(
      group = "Medicaid Financial Management Data",
      alias = c(
        "caid_finance_mgmt",
        "caid_finance_nation"
      )
    ),
    caid_nadac_group = list(
      group = "NADAC (National Average Drug Acquisition Cost)",
      alias = c(
        "caid_nadac",
        "caid_nadac_first",
        "caid_nadac_compare"
      )
    ),
    caid_benefit_pkg = list(
      group = "Benefit Package for Medicaid and CHIP Beneficiaries",
      alias = c(
        "benefit_pkg_month",
        "benefit_pkg_year"
      )
    ),
    caid_drug = list(
      group = "Medicaid Drug Datasets",
      alias = c(
        "drug_amp_mon",
        "drug_amp_qtr",
        "drug_products",
        "drug_clot",
        "drug_pediatric",
        "drug_contact_manu",
        "drug_contact_state"
      )
    ),
    caid_dual_status = list(
      group = "Dual Status Information for Medicaid and CHIP Beneficiaries",
      alias = c(
        "dual_status_month",
        "dual_status_year"
      )
    ),
    caid_meg = list(
      group = "Major Eligibility Group Information for Medicaid and CHIP Beneficiaries",
      alias = c(
        "meg_month",
        "meg_year"
      )
    ),
    caid_cms64 = list(
      group = "Medicaid CMS-64",
      alias = c(
        "cms64_caa",
        "cms64_ffcra",
        "cms64_adult"
      )
    ),
    caid_managed_care = list(
      group = "Medicaid Managed Care Enrollment",
      alias = c(
        "managed_care_summary",
        "managed_care_program_state",
        "managed_care_program_plan",
        "managed_care_program_pop_all",
        "managed_care_program_pop_dual",
        "managed_care_feat_pop",
        "managed_care_feat_qa",
        "managed_care_bene_month",
        "managed_care_bene_year"
      )
    ),
    caid_unwind = list(
      group = "Medicaid Unwinding Report",
      alias = c(
        "unwind_marketplace",
        "unwind_transition",
        "unwind_historic",
        "unwind_sbm"
      )
    ),
    cli_abort(c("x" = "{.emph group alias} {.val {alias}} is invalid."), call = call)
  )
  new_group(
    member_names = get_elem(x, "alias"),
    group_name   = get_elem(x, "group"),
    ...
  )
}
