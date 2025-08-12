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
        "provider_first_name",
        "covered_recipient_profile_first_name"
      ) ~ "first_name",
      field %in_% c(
        "provider_middle_name",
        "covered_recipient_profile_middle_name"
      ) ~ "middle_name",
      field %in_% c(
        "provider_last_name",
        "covered_recipient_profile_last_name"
      ) ~ "last_name",
      field %in_% c("provider_name", "recipient_name") ~ "full_name",
      field %in_% c("suff") ~ "suffix",
      field %in_% c("facility_name") ~ "facility_name",
      field %in_% c("measure_name") ~ "measure_name",
      field %in_% c("ind_pac_id") ~ "pac_ind",
      field %in_% c("org_pac_id") ~ "pac_org",
      field %in_% c("npi", "NPI", "Individual NPI", "covered_recipient_npi") ~ "npi",
      field %in_% c("entity_npi") ~ "npi_entity",
      field %in_% c("NPI - BUYER") ~ "npi_buyer",
      field %in_% c("NPI - SELLER") ~ "npi_seller",
      field %in_% c("Rfrg_NPI") ~ "npi_referring",
      field %in_% c("Rndrng_NPI") ~ "npi_rendering",
      field %in_% c("Suplr_NPI") ~ "npi_supplier",
      field %in_% c("Prscrbr_NPI", "PRSCRBR_NPI") ~ "npi_prescriber",
      field %in_% c(
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
        "city_name",
        "practicecity",
        "Geographic Location City Name"
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
        "Practice Location Phone Number"
      ) ~ "phone",
      field %in_% c(
        "zip",
        "zip_code",
        "practicezip9code"
        ) ~ "zip",
      field %in_% c(
        "address",
        "adr_ln_1",
        "address_line_1",
        "provider_address"
        ) ~ "address",
      field %in_% c("address_line_2", "adr_ln_2") ~ "address_2",
      field %in_% c(
        "state",
        "state_name",
        "state_or_nation",
        "practicestate",
        "practice state or us territory"
      ) ~ "state",
      .default = NA_character_
    )
  )
}
