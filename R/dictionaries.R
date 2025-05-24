# Research Payment Data
# Ownership Payment Data
# General Payment Data
# Covered Recipient Profile Supplement
#' @autoglobal
#' @noRd
open_dictionary <- function() {

  x <- fload("https://openpaymentsdata.cms.gov/api/1/metastore/schemas/dataset/items?show-reference-ids") |>
    get_elem("data", DF.as.list = TRUE) |>
    get_elem("title|describedBy$", regex = TRUE) |>
    map(\(x) x[!is.null(names(x))])

  new_df(
    name = get_elem(x, "title") |> delist(),
    download = get_elem(x, "describedBy") |> delist()) |>
    mtt(
      year = extract_year(name),
      name = ifelse(
        is_na(year),
        name,
        stri_extract_all_regex(
          name,
          "^.*(?=\\s.\\sDetailed Dataset [0-9]{4} Reporting Year)"
          )
        ),
        year = ifelse(
          is_na(year),
          fmax(year),
          year)
      ) |>
    sbt(year == fmax(year), -year) |>
    get_elem("download") |>
    map(request) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    map(function(resp)
      parse_string(resp, query = "/data") |>
        get_elem("fields") |>
        map_na_if() |>
        as_tbl() |>
        mtt(description = stri_trans_general(description, "latin-ascii"),
            description = gremove(description, "[\n\"']"),
            description = greplace(description, "[\\\\]", "-"),
            description = stri_trim_both(greplace(description, "\\s+", " ")),
            description = remove_non_ascii(description))
      ) |>
    set_names(
      get_elem(x, "title") |>
        stri_extract_all_regex(
          paste0(
            "^.*(?=\\s.\\sDetailed Dataset ",
            "[0-9]{4} Reporting Year)",
            "|Covered Recipient Profile Supplement")) |>
        unlist(use.names = FALSE) |>
        funique()
    ) |>
    list_rbind(names_to = "endpoint") |>
    slt(endpoint, field = name, description, format, constraints, title)
}

#' @autoglobal
#' @noRd
caid_dictionary <- function() {

  fload("https://data.medicaid.gov/api/1/metastore/schemas/dataset/items?show-reference-ids") |>
    get_elem("distribution") |>
    rowbind(fill = TRUE) |>
    get_elem("data") |>
    get_elem("describedBy") |>
    unlist(use.names = FALSE) |>
    funique() |>
    map(request) |>
    req_perform_parallel(on_error = "continue") |>
    map(function(resp) {

      x <- parse_string(resp, query = "/data")

      new_tbl(
        endpoint    = x$title,
        field       = x$fields$name,
        title       = x$fields$title,
        description = remove_non_ascii(greplace(x$fields$description, "\r\n", " ")),
        format      = x$fields$format) |>
        map_na_if()
      }
  ) |>
    list_rbind()
}

# State Drug Utilization Data Data Dictionary
# NADAC Data Dictionary
# NADAC 2021 Data Dictionary
# Drug Products Data Dictionary
# Monthly Enrollment Data Dictionary updated
# Drug Manufacturer Contact Info Data Dictionary
# Medicaid Enrollment - New Adult Group - Data Dictionary-12112024
# Quality Measures 2018 Dictionary
# Medicaid CMS-64 FFCRA Increased FMAP Expenditure data dictionary
# NADAC First Time Rates
# Medicaid CMS-64 FFCRA Increased FMAP Expenditure - Data Dictionary-12112024
# Medicaid Financial Management Data - Data Dictionary-12112024
# Quality Measures 2014-2015 Data Dictionary
# Medicaid Financial Management Data – National Totals  - Data Dictionary-12112024
# State Contact Information Data Dictionary
# AMP Reporting Monthly Data Dictionary
# Quality Measures 2016-2017 Data Dictionary
# NADAC Comparison Data Dictionary
# Federal Upper Limits Data Dictionary
# AMP Reporting Quarterly Data Dictionary
# Quality Measures 2019-2022 Data Dictionary
# Newly Reported Drugs Data Dictionary
# Division of Pharmacy Releases Index data dictionary
# Blood Disorders Data Dictionary
# Exclusively Pediatric Data Dictionary
# Clotting Factor Drug Report Data Dictionary
# Enrollment PUF (program information by month) Data Dictionary
# Enrollment PUF (program information by year) Data Dictionary
# Enrollment PUF (benefit package by month) Data Dictionary
# Enrollment PUF (benefit package by year) Data Dictionary
# Enrollment PUF (dual status information by month) Data Dictionary
# Enrollment PUF (dual status information by year) Data Dictionary
# Enrollment PUF (major eligibility group information by month) Data Dictionary
# Enrollment PUF (major eligibility group information by year) Data Dictionary
# Enrollment PUF (managed care information by month) Data Dictionary
# Enrollment PUF (managed care information by year) Data Dictionary
# Service Use PUF (Behavioral Health Services) Data Dictionary
# Service Use PUF (Blood Lead Screening Services) Data Dictionary
# Service Use PUF (Health Screening Services) Data Dictionary
# Service Use PUF (Contraceptive Care Services) Data Dictionary
# Service Use PUF (Telehealth Services) Data Dictionary
# Service Use PUF (Vaccinations Provided) Data Dictionary
# Number and rate of NAS per 1,000 births in newborns whose deliveries were covered by Medicaid or CHIP, 2017 - 2021
# Number and rate of SMM among Medicaid- and CHIP-covered deliveries, 2017 - 2021
# Beneficiaries who could benefit from integrated care, 2017-2021
# Beneficiaries receiving a physical health service among beneficiaries receiving a SUD service by physical health cond, 2017-2021
# Unwinding: Medicaid and CHIP CAA Reporting Metrics
# Unwinding: HealthCare.gov Transitions Marketplace Medicaid Unwinding Report
# Unwinding: State-based Marketplace (SBM) Medicaid Unwinding Report
# Unwinding: HealthCare.gov Marketplace Medicaid Unwinding Report
# Unwinding: Separate CHIP Enrollment by Month and State
# Medicaid CMS-64 CAA 2023 Increased FMAP Expenditure Data Collected through MBES/CBES - Data Dictionary-12112024
# Unwinding: Medicaid and CHIP Updated Renewal Outcomes
# Monthly enrollment PI dataset Data dictionary
# MLR Public Data Dictionary
# Renewal dataset data dictionary
# Current Separate CHIP Enrollment by Month and State Data Dictionary
# Race and ethnicity of the national Medicaid and CHIP population
# Medicaid enrollees who qualify for benefits based on disability
# Primary language spoken by the Medicaid and CHIP population
# Rural Medicaid and CHIP enrollees
# Section 1915(c) waiver program participants
# Medicaid and CHIP enrollees who received a well-child visit
# Medicaid and CHIP enrollees who received mental health or SUD services
# Prematurity and severe maternal morbidity among Medicaid- and CHIP-covered live births
# Medicaid Enterprise System Datatable - Data Dictionary
