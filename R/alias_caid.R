#' @autoglobal
#' @noRd
caid_main <- function(x, call = caller_env()) {
  x <- switch(
    x,
    mlr        = "MLR Summary Reports",
    mesd       = "Medicaid Enterprise System Datatable",
    wcv        = "Medicaid and CHIP enrollees who received a well-child visit",
    mhsud      = "Medicaid and CHIP enrollees who received mental health or SUD services",
    disability = "Medicaid enrollees who qualify for benefits based on disability",
    pi         = "PI dataset",
    livebirth  = "Prematurity and severe maternal morbidity among Medicaid- and CHIP-covered live births",
    blood      = "Pricing Comparison for Blood Disorder Treatments (Pricing as of 12/1/2024)",
    lang       = "Primary language spoken by the Medicaid and CHIP population",
    race       = "Race and ethnicity of the national Medicaid and CHIP population",
    rural      = "Rural Medicaid and CHIP enrollees",
    waive      = "Section 1915(c) waiver program participants",

    newdrug_01 = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program 01-06-2025-to-01-12-2025",
    newdrug_02 = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program 01-13-2025-to-01-19-2025",
    newdrug_03 = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program 01-20-2025-to-01-26-2025",
    newdrug_04 = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program 01-27-2025-to-02-02-2025",
    newdrug_05 = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program 02-03-2025-to-02-09-2025",
    newdrug_06 = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program 02-10-2025-to-02-16-2025",
    newdrug_07 = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program 02-17-2025-to-02-23-2025",
    newdrug_08 = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program 02-24-2025-to-03-02-2025",
    newdrug_09 = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program 03-03-2025-to-03-09-2025",
    newdrug_10 = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program 03-10-2025-to-03-16-2025",
    newdrug_11 = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program 03-17-2025-to-03-23-2025",
    newdrug_12 = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program 03-24-2025-to-03-30-2025",
    newdrug_13 = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program 03-31-2025-to-04-06-2025",
    newdrug_14 = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program 04-07-2025-to-04-13-2025",
    newdrug_15 = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program 04-14-2025-to-04-20-2025",
    newdrug_16 = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program 12-30-2024-to-01-05-2025",
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )

  if (!exists("catalog")) .catalog <- catalogs()

  select_alias(.catalog$caid$main, x) |> c()

}



#' @autoglobal
#' @noRd
caid_group <- function(x) {
  switch(
    x,
    MIPS = list(
      group = "PY 2022 MIPS Public Reporting",
      alias = c("MIPS_perform",
                "MIPS_patient",
                "MIPS_clinician",
                "MIPS_group",
                "MIPS_virtual")),
    PDC = list(
      group = "Provider Data Catalog",
      alias = c("affiliations",
                "clinicians",
                "utilization")),
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
}

#' @autoglobal
#' @noRd
caid_temp <- function(x, call = caller_env()) {

  x <- switch(
    x,
    quality_payment            = "^Quality Payment Program Experience$",
    IN_geography_and_service   = "^Medicare Inpatient Hospitals - by Geography and Service$",
    IN_provider                = "^Medicare Inpatient Hospitals - by Provider$",
    IN_provider_and_service    = "^Medicare Inpatient Hospitals - by Provider and Service$",
    OUT_geography_and_service  = "^Medicare Outpatient Hospitals - by Geography and Service$",
    OUT_provider_and_service   = "^Medicare Outpatient Hospitals - by Provider and Service$",
    PRX_geography_and_drug     = "^Medicare Part D Prescribers - by Geography and Drug$",
    PRX_provider               = "^Medicare Part D Prescribers - by Provider$",
    PRX_provider_and_drug      = "^Medicare Part D Prescribers - by Provider and Drug$",
    DME_geography_and_service  = "^Medicare Durable Medical Equipment, Devices & Supplies - by Geography and Service$",
    DME_provider               = "^Medicare Durable Medical Equipment, Devices & Supplies - by Referring Provider$",
    DME_provider_and_service   = "^Medicare Durable Medical Equipment, Devices & Supplies - by Referring Provider and Service$",
    DME_supplier               = "^Medicare Durable Medical Equipment, Devices & Supplies - by Supplier$",
    DME_supplier_and_service   = "^Medicare Durable Medical Equipment, Devices & Supplies - by Supplier and Service$",
    STAFF_non_nurse            = "^Payroll Based Journal Daily Non-Nurse Staffing$",
    STAFF_nurse                = "^Payroll Based Journal Daily Nurse Staffing$",
    STAFF_employee             = "^Payroll Based Journal Employee Detail Nursing Home Staffing$",
    UTIL_geography_and_service = "^Medicare Physician & Other Practitioners - by Geography and Service$",
    UTIL_provider              = "^Medicare Physician & Other Practitioners - by Provider$",
    UTIL_provider_and_service  = "^Medicare Physician & Other Practitioners - by Provider and Service$",
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call))

  if (!exists("catalog")) .catalog <- catalogs()

  x <- select_alias(.catalog$caid$temp, x)

  l <- slt(x, -data) |> c()

  list(
    title       = l$title,
    description = l$description,
    periodicity = l$periodicity,
    contact     = l$contact,
    dictionary  = l$dictionary,
    site        = l$site,
    identifier  = get_elem(x, "data")[[1]]$identifier[1],
    endpoints   = get_elem(x, "data")[[1]]
  )

}
