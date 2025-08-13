#' @autoglobal
#' @noRd
alias_column <- function(df, aka) {
  to_col <- function(aka) {
    code_head  <- glue::as_glue("cheapr::case(\n")
    code_tail  <- glue::as_glue(",\n .default = NA)")

    string <- paste0(
      "gdetect(title, ",
      "{glue::single_quote(unname(x))}) ~ ",
      "{glue::single_quote(names(x))}"
    )

    code_head +
      glue::glue(string, x = aka) |>
      glue::glue_collapse(sep = ",\n") +
      code_tail
  }

  collapse::mtt(
    df,
    alias = to_col(aka) |>
      rlang::parse_expr() |>
      rlang::eval_bare()
  )
}

#' @autoglobal
#' @noRd
alias_after <- function(df, aka) {
  to_col <- function(aka) {
    code_head  <- glue::as_glue("cheapr::case(\n")
    code_tail  <- glue::as_glue(",\n .default = alias)")

    string <- paste0(
      "gdetect(title, ",
      "{glue::single_quote(unname(x))}) ~ ",
      "{glue::single_quote(names(x))}"
    )

    code_head +
      glue::glue(string, x = aka) |>
      glue::glue_collapse(sep = ",\n") +
      code_tail
  }

  collapse::mtt(
    df,
    alias = to_col(aka) |>
      rlang::parse_expr() |>
      rlang::eval_bare()
  )
}

# get_pin("field_types") |> field_type_col()
#' @autoglobal
#' @noRd
field_type_col <- function(df) {
  collapse::mtt(
    df,
    field = rm_nonascii(field),
    type = cheapr::case(
      field %in_% c(
        "FIRST_NAME",
        "First Name",
        "provider_first_name",
        "covered_recipient_profile_first_name",
        "Individual First Name"
      ) ~ "first_name",
      field %in_% c(
        "MIDDLE NAME",
        "MDL_NAME",
        "provider_middle_name",
        "covered_recipient_profile_middle_name"
      ) ~ "middle_name",
      field %in_% c(
        "LAST_NAME",
        "Last Name",
        "provider_last_name",
        "covered_recipient_profile_last_name",
        "Individual Last Name"
      ) ~ "last_name",
      field %in_% c(
        "provider_name",
        "recipient_name",
        "PRVDR_NAME"
        ) ~ "full_name",
      field %in_% c("suff") ~ "suffix_name",
      field %in_% c("ORG_NAME", "ORGANIZATION NAME", "Organization Name") ~ "org_name",
      field %in_% c("FAC_NAME", "facility_name") ~ "facility_name",
      field %in_% c("measure_name") ~ "measure_name",
      field %in_% c("DOING BUSINESS AS NAME", "Group Legal Business Name") ~ "dba_name",
      field %in_% c("Hosp_Name") ~ "hospital_name",

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

      field %in_% c("hcpcs_cd") ~ "hcpcs",

      field %in_% c(
        "CLIA_ID_NUMBER_1",
        "CLIA_ID_NUMBER_2",
        "CLIA_ID_NUMBER_3",
        "CLIA_ID_NUMBER_4"
        ) ~ "clia",

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
      field %in_% c("Rfrg_NPI") ~ "npi_referring",
      field %in_% c("Rndrng_NPI") ~ "npi_rendering",
      field %in_% c("Suplr_NPI") ~ "npi_supplier",
      field %in_% c("Prscrbr_NPI", "PRSCRBR_NPI") ~ "npi_prescriber",

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
      field %in_% c("Rndrng_Prvdr_CCN") ~ "ccn_rendering",
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
      field %in_% c("PRVDR_CITY") ~ "city_provider",
      field %in_% c("Rfrg_Prvdr_City") ~ "city_referring",
      field %in_% c("Rndrng_Prvdr_City") ~ "city_rendering",
      field %in_% c("Suplr_Prvdr_City") ~ "city_supplier",
      field %in_% c("Prscrbr_City") ~ "city_prescriber",
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
      field %in_% c("country", "country_name") ~ "country",
      field %in_% c(
        "phone",
        "Phone",
        "PHONE",
        "Telephone Number",
        "telephone_number",
        "telephonenumber",
        "Practice Location Phone Number",
        "PHNE_NUM"
      ) ~ "phone",
      field %in_% c(
        "zip",
        "zip_code",
        "practicezip9code",
        "ZIP_CD",
        "Zip code",
        "ZIP CODE",
        "PRVDR_ZIP"
        ) ~ "zip",
      field %in_% c(
        "address",
        "adr_ln_1",
        "address_line_1",
        "provider_address",
        "ST_ADR",
        "First Line Street Address",
        "ADDRESS LINE 1",
        "Hosp_Address"
        ) ~ "address",
      field %in_% c(
        "address_line_2",
        "adr_ln_2",
        "Second Line Street Address",
        "ADDRESS LINE 2"
        ) ~ "address_2",
      field %in_% c(
        "State Code",
        "STATE_CD",
        "state",
        "State",
        "state_name",
        "state_or_nation",
        "practicestate",
        "practice state or us territory"
      ) ~ "state",
      field %in_% c("Group State Code") ~ "state_org",
      field %in_% c("Individual State Code", "Enrollment State Code") ~ "state_ind",
      .default = NA_character_
    )
  )
}
