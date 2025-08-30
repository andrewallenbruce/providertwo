#' @autoglobal
#' @noRd
alias_column <- function(df, aka, default = "NA") {

  default <- match.arg(default, c("NA", "alias"))

  to_col <- function(aka) {
    code_def  <- glue::glue(",\n .default = {default})")

    string <- paste0(
      "gdetect(title, ",
      "{glue::single_quote(unname(x))}) ~ ",
      "{glue::single_quote(names(x))}"
    )

    glue::as_glue("cheapr::case(\n") +
      glue::glue(string, x = aka) |>
      glue::glue_collapse(sep = ",\n") +
      code_def
  }

  collapse::mtt(
    df,
    alias = to_col(aka) |>
      rlang::parse_expr() |>
      rlang::eval_bare()
  )
}

#' Field Dictionary
#' @examples
#' dictionary()
#' @returns A data frame
#' @autoglobal
#' @export
dictionary <- function() {
  get_pin("dicts")
}

#' Field Table
#' @examples
#' field_table()
#' @returns A data frame
#' @autoglobal
#' @export
field_table <- function() {

  collapse::mtt(
    get_pin("field_types"),
    field = rm_nonascii(field),
    type = cheapr::case(
      field %in_% c(
        "FIRST_NAME",
        "FIRST NAME - OWNER",
        "First Name",
        "provider_first_name",
        "covered_recipient_profile_first_name",
        "Individual First Name",
        "Prscrbr_First_Name",
        "Rfrg_Prvdr_First_Name"
      ) ~ "first_name",
      field %in_% c(
        "MIDDLE NAME",
        "MIDDLE NAME - OWNER",
        "MDL_NAME",
        "provider_middle_name",
        "covered_recipient_profile_middle_name",
        "Rfrg_Prvdr_MI"
      ) ~ "middle_name",
      field %in_% c(
        "LAST_NAME",
        "LAST NAME - OWNER",
        "Last Name",
        "provider_last_name",
        "covered_recipient_profile_last_name",
        "Individual Last Name",
        "Prscrbr_Last_Org_Name",
        "Rfrg_Prvdr_Last_Name_Org"
      ) ~ "last_name",
      field %in_% c("suff") ~ "suffix",
      field %in_% c(
        "ORG_NAME",
        "ORGANIZATION NAME",
        "ORGANIZATION NAME - OWNER",
        "Organization Name",
        "provider_name",
        "recipient_name",
        "PRVDR_NAME",
        "Prscrbr_Last_Org_Name",
        "Rfrg_Prvdr_Last_Name_Org"
        ) ~ "org_name",
      field %in_% c(
        "FAC_NAME",
        "facility_name"
        ) ~ "facility_name",
      field %in_% c(
        "DOING BUSINESS AS NAME",
        "Group Legal Business Name",
        "DOING BUSINESS AS NAME - OWNER"
        ) ~ "dba_name",
      field %in_% c("Hosp_Name") ~ "hospital_name",
      field %in_% c("ACO_Name", "aco_name") ~ "aco_name",
      field %in_% c("Brnd_Name") ~ "brand_name",

      field %in_% c(
        "PROVIDER_TYPE_CD",
        "PROVIDER TYPE CODE"
        ) ~ "specialty_code",
      field %in_% c(
        "Specialty",
        "PROVIDER_TYPE_DESC",
        "PROVIDER TYPE TEXT",
        "Provider Type Text",
        "Individual Specialty Description"
        ) ~ "specialty",

      field %in_% c(
        "ENRLMT_ID",
        "ENROLLMENT ID",
        "Enrollment ID",
        "Individual Enrollment ID"
        ) ~ "enid_ind",
      field %in_% c(
        "Group Enrollment ID"
      ) ~ "enid_org",

      field %in_% c(
        "PECOS_ASCT_CNTL_ID",
        "ind_pac_id",
        "ASSOCIATE ID",
        "Individual PAC ID"
        ) ~ "pac_ind",
      field %in_% c("org_pac_id", "Group PAC ID") ~ "pac_org",
      field %in_% c("ASSOCIATE ID - OWNER") ~ "pac_owner",

      field %in_% c("hcpcs_cd", "HCPCS_Cd", "HCPCS_CD") ~ "hcpcs",
      field %in_% c("HCPCS_Desc") ~ "hcpcs_description",

      field %in_% c("DRG_Cd") ~ "drg",
      field %in_% c("DRG_Desc") ~ "drg_description",

      field %in_% c(
        "npi",
        "NPI",
        "Individual NPI",
        "covered_recipient_npi",
        "National Provider Identifier"
        ) ~ "npi",
      field %in_% c("entity_npi") ~ "npi_entity",
      field %in_% c("NPI - BUYER") ~ "npi_buyer",
      field %in_% c("NPI - SELLER") ~ "npi_seller",
      field %in_% c("Rfrg_NPI") ~ "npi_refer",
      field %in_% c("Rndrng_NPI") ~ "npi_render",
      field %in_% c("Suplr_NPI") ~ "npi_supply",
      field %in_% c("Prscrbr_NPI", "PRSCRBR_NPI") ~ "npi_prx",
      field %in_% c("MULTIPLE NPI FLAG") ~ "multi_npi",

      field %in_% c(
        "PRVDR_NUM",
        "alternate_ccn",
        "Alternate_CCNs",
        "ccn",
        "CCN",
        "Provider CCN",
        "cms_certification_number_ccn",
        "CAH OR HOSPITAL CCN",
        "HHA-based Hospice Provider CCN"
      ) ~ "ccn",
      field %in_% c("CCN - BUYER") ~ "ccn_buyer",
      field %in_% c("CCN - SELLER") ~ "ccn_seller",
      field %in_% c("Rndrng_Prvdr_CCN") ~ "ccn_render",
      field %in_% c(
        "alternate_ccn",
        "Alternate_CCNs"
        ) ~ "ccn_alt",

      field %in_% c(
        "year",
        "Year",
        "YEAR",
        "perf_year",
        "Performance_Year",
        "PERF_YEAR",
        "fiscal_year",
        "payment_year",
        "Calendar Year"
      ) ~ "year",
      field %in_% c(
        "years",
        "Years",
        "YEARS",
        "years in medicare"
        ) ~ "years",
      field %in_% c(
        "city",
        "City",
        "CITY",
        "citytown",
        "CITY_NAME",
        "City_Name",
        "City Name",
        "city_name",
        "practicecity",
        "Geographic Location City Name",
        "PRVDR_CITY"
      ) ~ "city",
      field %in_% c("CITY - OWNER") ~ "city_owner",
      field %in_% c("PRVDR_CITY") ~ "city_org",
      field %in_% c("Rfrg_Prvdr_City") ~ "city_refer",
      field %in_% c("Rndrng_Prvdr_City") ~ "city_render",
      field %in_% c("Suplr_Prvdr_City") ~ "city_supply",
      field %in_% c("Prscrbr_City") ~ "city_prx",
      field %in_% c(
        "county",
        "County",
        "county_name",
        "COUNTY_NAME",
        "County_Name",
        "County Name",
        "State County Name",
        "countyparish"
      ) ~ "county",
      field %in_% c(
        "country",
        "country_name",
        "Rfrg_Prvdr_Cntry"
        ) ~ "country",
      field %in_% c(
        "phone",
        "Phone",
        "PHONE",
        "Telephone Number",
        "telephone_number",
        "telephonenumber",
        "Practice Location Phone Number",
        "PHNE_NUM",
        "FAX_PHNE_NUM"
      ) ~ "phone",
      field %in_% c(
        "zip",
        "zip_code",
        "practicezip9code",
        "ZIP_CD",
        "Zip code",
        "Zip Code",
        "ZIP CODE",
        "ZIP CODE - OWNER",
        "PRVDR_ZIP",
        "Rfrg_Prvdr_Zip5"
        ) ~ "zip",
      field %in_% c(
        "address",
        "adr_ln_1",
        "address_line_1",
        "provider_address",
        "ST_ADR",
        "First Line Street Address",
        "ADDRESS LINE 1",
        "ADDRESS LINE 1 - OWNER",
        "Hosp_Address",
        "Street Address",
        "Rndrng_Prvdr_St1",
        "Rfrg_Prvdr_St1"
        ) ~ "address",
      field %in_% c(
        "address_line_2",
        "adr_ln_2",
        "Second Line Street Address",
        "ADDRESS LINE 2",
        "ADDRESS LINE 2 - OWNER",
        "Rndrng_Prvdr_St2",
        "Rfrg_Prvdr_St2"
        ) ~ "address_2",

      field %in_% c(
        "ENROLLMENT STATE",
        "INCORPORATION STATE",
        "Prscrbr_State_Abrvtn",
        "Rfrg_Prvdr_State_Abrvtn",
        "State Code",
        "STATE_CD",
        "STATE - OWNER",
        "STATE_RGN_CD",
        "STATE",
        "state",
        "State",
        "state_name",
        "state_or_nation",
        "practicestate",
        "practice state or us territory"
      ) ~ "state",

      field %in_% c("Group State Code") ~ "state_org",
      field %in_% c("Individual State Code", "Enrollment State Code") ~ "state_ind",

      field %in_% c("WorkDate") ~ "work_date",
      field %in_% c(
        "start_date",
        "current_start_date",
        "initial_start_date",
        "Fiscal Year Begin Date"
        ) ~ "start_date",
      field %in_% c(
        "end_date",
        "Fiscal Year End Date"
        ) ~ "end_date",
      field %in_% c("effective_date") ~ "effective_date",
      field %in_% c("termination_date") ~ "termination_date",
      field %in_% c("processing_date") ~ "process_date",
      field %in_% c("fda_approval_date") ~ "fda_approval_date",
      field %in_% c("market_date") ~ "market_date",
      field %in_% c("measure_date_range") ~ "measure_date_range",
      field %in_% c("measure_abbreviation") ~ "measure_abb",
      field %in_% c("as_of_date") ~ "asof_date",
      field %in_% c("INCORPORATION DATE") ~ "incorp_date",

      field %in_% c("month") ~ "month",
      field %in_% c("quarter") ~ "quarter",

      field %in_% c("ndc") ~ "ndc",
      field %in_% c("ndc_description") ~ "ndc_description",
      field %in_% c("package_size") ~ "pkg_size",
      field %in_% c("product_name") ~ "product_name",
      field %in_% c("product_code") ~ "product_code",
      field %in_% c("labeler_code") ~ "labeler_code",
      field %in_% c("labeler_name") ~ "labeler_name",
      field %in_% c("explanation_code") ~ "explanation_code",

      field %in_% c("suppression_used") ~ "suppressed",
      field %in_% c("number_of_prescriptions") ~ "prescriptions",
      field %in_% c("classification_for_rate_setting") ~ "rate_class",
      field %in_% c("state_rate") ~ "rate_state",
      field %in_% c("rateper1000beneficiaries") ~ "rate_bene",
      field %in_% c("population") ~ "population",

      field %in_% c("nadac_per_unit") ~ "nadac_per_unit",
      field %in_% c("pricing_unit") ~ "pricing_unit",
      field %in_% c("unit_type") ~ "unit_type",
      field %in_% c("drug_type") ~ "drug_type",
      field %in_% c("pharmacy_type_indicator") ~ "pharmacy_type",

      field %in_% c("units_reimbursed") ~ "units_reimbursed",
      field %in_% c("total_amount_reimbursed") ~ "total_reimbursed",
      field %in_% c("medicaid_amount_reimbursed") ~ "medicaid_reimbursed",
      field %in_% c("non_medicaid_amount_reimbursed") ~ "non_medicaid_amount_reimbursed",

      field %in_% c("corresponding_generic_drug_nadac_per_unit") ~ "generic_nadac_per_unit",
      field %in_% c("corresponding_generic_drug_effective_date") ~ "generic_effective_date",
      field %in_% c("utilization_type") ~ "utilization_type",

      field %in_% c("score") ~ "score",
      field %in_% c("otc") ~ "otc",
      field %in_% c("cms_region") ~ "region",
      field %in_% c("network") ~ "network",
      field %in_% c("facility_id") ~ "facility_id",

      field %in_% c("measure_name") ~ "measure_name",
      field %in_% c("measure_id") ~ "measure_id",
      field %in_% c("measure_code") ~ "measure_code",

      field %in_% c("number_of_completed_surveys") ~ "surveys_complete",
      field %in_% c("number_of_beneficiaries") ~ "beneficiaries",
      field %in_% c("count_of_enrollees") ~ "enrollees",
      field %in_% c("servicecount") ~ "services",
      field %in_% c("number_of_states_reporting") ~ "states",
      field %in_% c("Number of Beds") ~ "beds",

      field %in_% c("Place_Of_Srvc") ~ "pos",

      field %in_% c("median") ~ "median",
      field %in_% c("location") ~ "location",

      .default = NA_character_
    ) |>
      rm_nonascii()
  )
}
