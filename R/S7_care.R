#' @include S7_classes.R
NULL

#' Medicare API Endpoint Classes
#' @name medicare
#' @param alias `<chr>` endpoint alias
#' @param call `<env>` environment to use for error reporting
#' @param ... Additional arguments passed to the group constructor
#' @returns An S7 `<care_endpoint>`, or `<care_temporal>` object
#' @examples
#' care_endpoint("care_dialysis")
#' care_temporal("quality_payment")
#' care_group("care_hospital")
NULL

#' @autoglobal
#' @rdname medicare
#' @export
care_endpoint <- new_class(
  name        = "care_endpoint",
  parent      = class_endpoint,
  package     = NULL,
  constructor = function(alias) {

    x <- select_care(alias)

    new_object(
      class_endpoint(),
      identifier  = x$identifier,
      metadata    = get_metadata(x),
      dimensions  = care_dimensions(x)
    )
  }
)

#' @autoglobal
#' @rdname medicare
#' @export
care_temporal <- new_class(
  name        = "care_temporal",
  parent      = class_temporal,
  package     = NULL,
  constructor = function(alias) {

    x <- select_care_temp(alias)

    new_object(
      class_temporal(),
      metadata    = get_metadata(x),
      dimensions  = care_dimensions(x),
      endpoints   = x$endpoints
    )
  }
)

#' @autoglobal
#' @rdname medicare
#' @export
care_group <- function(alias, call = caller_env(), ...) {
  x <- switch(
    alias,
    care_hha = list(
      group = "Home Health Agencies",
      alias = c(
        "hha_owners",
        "hha_costreport",
        "hha_enrollments"
      )
    ),
    care_hospice = list(
      group = "Hospices",
      alias = c(
        "hospice_owners",
        "hospice_enrollments",
        "hospice_acute"
      )
    ),
    care_hospital = list(
      group = "Hospitals",
      alias = c(
        "hospital_owners",
        "hospital_chow",
        "hospital_chow_owner",
        "hospital_enrollments",
        "hospital_costreport",
        "hospital_service_area"
      )
    ),
    rhc = list(
      group = "Rural Health Clinics",
      alias = c(
        "rhc_owners",
        "rhc_enrollments"
      )
    ),
    fqhc = list(
      group = "Federally Qualified Health Centers",
      alias = c(
        "fqhc_owners",
        "fqhc_enrollments"
      )
    ),
    pending = list(
      group = "Pending Initial Logging and Tracking",
      alias = c(
        "pilat_non_physician",
        "pilat_physician"
      )
    ),
    reassignment = list(
      group = "Revalidation Reassignment Lists",
      alias = c(
        "revalid_group",
        "revalid_due",
        "revalid_list"
      )
    ),
    care_snf = list(
      group = "Skilled Nursing Facilities",
      alias = c(
        "snf_owners",
        "snf_chow",
        "snf_chow_owner",
        "snf_cost_report",
        "snf_enrollments"
      )
    ),
    care_aco = list(
      group = "Accountable Care Organizations",
      alias = c(
        "aco_reach_aligned",
        "aco_reach_eligible",
        "aco_reach_results",
        "aco_reach_providers",
        "aco_reach_orgs",
        "aco_pioneer",
        "aco_participants",
        "aco_snf_affiliate",
        "aco_organizations",
        "aco_bene_cnty"
      )
    ),
    cms_stats = list(
      group = "CMS Program Statistics",
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
      group = "Medicaid",
      alias = c(
        "care_caid_managed_care",
        "care_caid_opioid_geo",
        "care_caid_drug_spend"
      )
    ),
    care_geovar = list(
      group = "Medicare Geographic Variation",
      alias = c(
        "geovar_adv",
        "geovar_hrr",
        "geovar_nsc"
      )
    ),
    care_pdp = list(
      group = "Prescription Drug Plan Formulary, Pharmacy Network, and Pricing Information",
      alias = c(
        "pdp_month",
        "pdp_quarter"
      )
    ),
    bene_survey = list(
      group = "Medicare Current Beneficiary Survey",
      alias = c(
        "bene_survey_covid",
        "bene_survey_cost",
        "bene_survey_file"
      )
    ),
    partb_drug = list(
      group = "Medicare Part B Drugs",
      alias = c(
        "partb_drug_discard",
        "partb_drug_spend"
      )
    ),
    partd_drug = list(
      group = "Medicare Part D Drugs",
      alias = c(
        "partd_opioid",
        "partd_drug_spend"
      )
    ),
    care_market = list(
      group = "Market Saturation & Utilization",
      alias = c(
        "market_cbsa",
        "market_state_cnty"
      )
    ),
    care_inpatient = list(
      group = "Medicare Inpatient Hospitals",
      alias = c(
        "inpatient_geography",
        "inpatient_provider",
        "inpatient_service"
      )
    ),
    care_outpatient = list(
      group = "Medicare Outpatient Hospitals",
      alias = c(
        "outpatient_geography",
        "outpatient_service"
      )
    ),
    care_prescribers = list(
      group = "Medicare Part D Prescribers",
      alias = c(
        "prx_geography",
        "prx_provider",
        "prx_drug"
      )
    ),
    care_dme_suppliers = list(
      group = "Medicare Durable Medical Equipment, Devices & Supplies",
      alias = c(
        "dme_geography",
        "dme_provider",
        "dme_service",
        "dme_supplier",
        "dme_supplier_service"
      )
    ),
    care_nhome_staff = list(
      group = "Nursing Home Payroll-Based Journal Staffing",
      alias = c(
        "nhome_staff_nonurse",
        "nhome_staff_nurse",
        "nhome_staff_employee"
      )
    ),
    care_utilization = list(
      group = "Medicare Physician & Other Practitioners",
      alias = c(
        "util_geography",
        "util_provider",
        "util_service"
      )
    ),
    care_nhome = list(
      group = "Nursing Home Performance",
      alias = c(
        "nhome_performance",
        "nhome_mds_frequency",
        "nhome_mds_facility"
      )
    ),
    cli_abort(c("x" = "{.emph group alias} {.val {alias}} is invalid."), call = call)
  )

  new_group(
    member_names = x$alias,
    group_name   = x$group,
    ...
  )
}

