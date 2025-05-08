#' @autoglobal
#' @noRd
select_care_group <- function(x, call = caller_env()) {
  switch(
    x,
    HHA = list(
      group = "Home Health Agencies",
      alias = c(
        "HHA_owners",
        "HHA_cost_report",
        "HHA_enrollments"
      )
    ),
    hospice = list(
      group = "Hospices",
      alias = c(
        "SPICE_owners",
        "SPICE_enrollments",
        "SPICE_acute"
      )
    ),
    hospital = list(
      group = "Hospitals",
      alias = c(
        "hospital_owners",
        "hospital_chow",
        "hospital_chow_owner",
        "hospital_enrollments",
        "hospital_cost_report",
        "hospital_service_area"
      )
    ),
    RHC = list(
      group = "Rural Health Clinics",
      alias = c(
        "RHC_owners",
        "RHC_enrollments"
      )
    ),
    FQHC = list(
      group = "Federally Qualified Health Centers",
      alias = c(
        "FQHC_owners",
        "FQHC_enrollments"
      )
    ),
    pending = list(
      group = "Pending Initial Logging and Tracking",
      alias = c(
        "PILAT_non_physicians",
        "PILAT_physicians"
      )
    ),
    reassignment = list(
      group = "Revalidation Reassignment Lists",
      alias = c(
        "REVAL_group",
        "REVAL_due_date",
        "REVAL_reassign"
      )
    ),
    SNF = list(
      group = "Skilled Nursing Facilities",
      alias = c(
        "SNF_owners",
        "SNF_chow",
        "SNF_chow_owner",
        "SNF_cost_report",
        "SNF_enrollments"
      )
    ),
    ACO = list(
      group = "Accountable Care Organizations",
      alias = c(
        "ACO_pioneer",
        "ACO_participants",
        "ACO_SNF",
        "ACO_orgs",
        "ACO_bene",
        "ACO_REACH_align",
        "ACO_REACH_elig",
        "ACO_REACH_fin",
        "ACO_REACH_prov",
        "ACO_REACH_orgs"
      )
    ),
    program_stats = list(
      group = "CMS Program Statistics",
      alias = c(
        "CMS_MA_enroll",
        "CMS_MA_outpatient",
        "CMS_MA_other",
        "CMS_MA_inpatient",
        "CMS_MA_SNF",
        "CMS_deaths",
        "CMS_HHA",
        "CMS_spice",
        "CMS_inpatient",
        "CMS_new_enroll",
        "CMS_outpatient",
        "CMS_TOS",
        "CMS_partD",
        "CMS_partD_enroll",
        "CMS_other",
        "CMS_premiums",
        "CMS_providers",
        "CMS_SNF",
        "CMS_total_enroll",
        "CMS_dual_enroll",
        "CMS_orig_enroll"
      )
    ),
    CAID = list(
      group = "Medicaid",
      alias = c(
        "CAID_managed",
        "CAID_opioid",
        "CAID_drug"
      )
    ),
    geo_variation = list(
      group = "Medicare Geographic Variation",
      alias = c(
        "GEO_MA",
        "GEO_HRR",
        "GEO_NSC"
      )
    ),
    PDP = list(
      group = "Prescription Drug Plan Formulary, Pharmacy Network, and Pricing Information",
      alias = c(
        "PDP_month",
        "PDP_quarter"
      )
    ),
    BENE_surv = list(
      group = "Medicare Current Beneficiary Survey",
      alias = c(
        "BENE_surv_covid",
        "BENE_surv_cost",
        "BENE_surv_file"
      )
    ),
    PTB_drugs = list(
      group = "Medicare Part B Drugs",
      alias = c(
        "PTB_discard",
        "PTB_spending"
      )
    ),
    MKT = list(
      group = "Market Saturation & Utilization",
      alias = c(
        "MKT_cbsa",
        "MKT_state_cnty"
      )
    ),
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
}

#' @autoglobal
#' @noRd
select_care_troup <- function(x, call = caller_env()) {
  switch(
    x,
    inpatient   = list(
      group = "Medicare Inpatient Hospitals",
      alias = c(
        "IN_geography",
        "IN_provider",
        "IN_service"
      )
    ),
    outpatient  = list(
      group = "Medicare Outpatient Hospitals",
      alias = c(
        "OUT_geography",
        "OUT_service"
      )
    ),
    prescribers = list(
      group = "Medicare Part D Prescribers",
      alias = c(
        "PRX_geography",
        "PRX_provider",
        "PRX_drug"
      )
    ),
    suppliers   = list(
      group = "Medicare Durable Medical Equipment, Devices & Supplies",
      alias = c(
        "DME_geography",
        "DME_provider",
        "DME_service",
        "DME_supplier",
        "DME_supplier_service"
      )
    ),
    staffing    = list(
      group = "Nursing Home Payroll-Based Journal Staffing",
      alias = c(
        "NH_staff_nonurse",
        "NH_staff_nurse",
        "NH_staff_employee"
      )
    ),
    utilization = list(
      group = "Medicare Physician & Other Practitioners",
      alias = c(
        "UTIL_geography",
        "UTIL_provider",
        "UTIL_service"
      )
    ),
    care_nursing = list(group = "Nursing Home Performance", alias = c(
        "NH_performance",
        "NH_mds_frequency",
        "NH_mds_facility"
      )
    ),
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
}
