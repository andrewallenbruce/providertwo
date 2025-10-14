#' @autoglobal
#' @noRd
FIELD <- list(
  #NAME####
  first_name = c(
    "FIRST_NAME",
    "FIRST NAME - OWNER",
    "First Name",
    "provider_first_name",
    "covered_recipient_profile_first_name",
    "Individual First Name",
    "Prscrbr_First_Name",
    "Rfrg_Prvdr_First_Name"
  ),
  middle_name = c(
    "MIDDLE NAME",
    "MIDDLE NAME - OWNER",
    "MDL_NAME",
    "provider_middle_name",
    "covered_recipient_profile_middle_name",
    "Rfrg_Prvdr_MI"
  ),
  last_name = c(
    "LAST_NAME",
    "LAST NAME - OWNER",
    "Last Name",
    "provider_last_name",
    "covered_recipient_profile_last_name",
    "Individual Last Name",
    "Prscrbr_Last_Org_Name",
    "Rfrg_Prvdr_Last_Name_Org"
  ),
  org_name = c(
    "ORG_NAME",
    "ORGANIZATION NAME",
    "ORGANIZATION NAME - OWNER",
    "Organization Name",
    "recipient_name",
    "Prscrbr_Last_Org_Name",
    "Rfrg_Prvdr_Last_Name_Org"
  ),
  provider_name = c(
    "provider_name",
    "Provider_Name",
    "Provider Name",
    "provname",
    "PRVDR_NAME"
    ),
  suffix        = c("suff"),
  facility      = c("FAC_NAME", "facility_name"),
  measure       = c("measure_name", "Measure", "_label_"),
  measure_score = c("Measure Score", "Measure_Score", "col1"),
  measure_id    = c("Measure_ID", "Measure ID", "measure_id"),
  issuer        = c("issuer_name"),
  dba_name      = c("DOING BUSINESS AS NAME",
                    "Group Legal Business Name",
                    "DOING BUSINESS AS NAME - OWNER"),
  hospital_name = c("Hosp_Name"),
  aco_name      = c("ACO_NAME", "aco_name"),
  brand_name    = c("Brand Name", "Brnd_Name"),

  #SPECIALTY####
  specialty_code = c("PROVIDER_TYPE_CD", "PROVIDER TYPE CODE"),
  specialty = c(
    "Specialty",
    "PROVIDER_TYPE_DESC",
    "PROVIDER TYPE TEXT",
    "Provider Type Text",
    "Individual Specialty Description",
    "clinician specialty"
  ),

  #IDENTIFIER####
  ##__NPI####
  npi = c(
    "npi",
    "NPI",
    "Individual NPI",
    "covered_recipient_npi",
    "National Provider Identifier"
  ),
  npi_entity  = c("entity_npi"),
  npi_buyer   = c("NPI - BUYER"),
  npi_seller  = c("NPI - SELLER"),
  npi_refer   = c("Rfrg_NPI"),
  npi_render  = c("Rndrng_NPI"),
  npi_supply  = c("Suplr_NPI"),
  npi_scribe  = c("Prscrbr_NPI", "PRSCRBR_NPI"),

  ##__ENID####
  enid_ind = c("ENRLMT_ID",
               "ENROLLMENT ID",
               "Enrollment ID",
               "Individual Enrollment ID"),
  enid_org = c("Group Enrollment ID"),

  ##__PAC####
  pac_ind = c(
    "PECOS_ASCT_CNTL_ID",
    "ind_pac_id",
    "ASSOCIATE ID",
    "Individual PAC ID"),
  pac_org = c("org_pac_id", "Group PAC ID"),
  pac_own = c("ASSOCIATE ID - OWNER"),

  ##__CCN####
  ccn = c(
    "PRVDR_NUM",
    "alternate_ccn",
    "Alternate_CCNs",
    "ccn",
    "CCN",
    "Provider CCN",
    "provfs",
    "cms_certification_number_ccn",
    "CAH OR HOSPITAL CCN",
    "HHA-based Hospice Provider CCN"
  ),
  ccn_alt = c("alternate_ccn",
              "Alternate_CCNs",
              "alt_ccns",
              "Alternate CCN(s)"),
  ccn_buy = c("CCN - BUYER"),
  ccn_sell = c("CCN - SELLER"),
  ccn_render = c("Rndrng_Prvdr_CCN"),

  #TERMINOLOGY####
  ##__HCPCS####
  hcpcs = c("hcpcs_cd", "HCPCS_Cd", "HCPCS_CD"),
  hcpcs_desc = c("HCPCS_Desc", "HCPCS_DESC"),
  ##__DRG####
  drg = c("DRG_Cd"),
  drg_desc = c("DRG_Desc"),

  #LOCATION####
  ##__CITY####
  city = c(
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
    "provcity",
    "PRVDR_CITY"
  ),
  city_own = c("CITY - OWNER"),
  city_refer = c("Rfrg_Prvdr_City"),
  city_render = c("Rndrng_Prvdr_City"),
  city_supply = c("Suplr_Prvdr_City"),
  city_scribe = c("Prscrbr_City"),

  ##__COUNTY####
  county = c(
    "county",
    "County",
    "county_name",
    "COUNTY_NAME",
    "County_Name",
    "County Name",
    "State County Name",
    "countyparish"),
  ##__COUNTRY####
  country = c(
    "country",
    "country_name",
    "Rfrg_Prvdr_Cntry"
    ),
  ##__PHONE####
  phone = c(
    "phone",
    "Phone",
    "PHONE",
    "Telephone Number",
    "telephone_number",
    "telephonenumber",
    "Practice Location Phone Number",
    "PHNE_NUM",
    "FAX_PHNE_NUM"
  ),
  ##__ADDRESS####
  address = c(
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
  ),
  ##__ADDRESS_2####
  address_2 = c(
    "address_line_2",
    "adr_ln_2",
    "Second Line Street Address",
    "ADDRESS LINE 2",
    "ADDRESS LINE 2 - OWNER",
    "Rndrng_Prvdr_St2",
    "Rfrg_Prvdr_St2"
    ),
  ##__STATE####
  state = c(
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
    "state_code",
    "state_or_nation",
    "practicestate",
    "practice state or us territory"
  ),
  state_org = c("Group State Code"),
  state_ind = c("Individual State Code", "Enrollment State Code"),

  ##__ZIP####
  zip = c(
    "zip",
    "zip_code",
    "practicezip9code",
    "ZIP_CD",
    "Zip code",
    "Zip Code",
    "ZIP CODE",
    "ZIP CODE - OWNER",
    "PRVDR_ZIP",
    "Rfrg_Prvdr_Zip5",
    "zipcodes"
    ),

  #COUNTS####
  years           = c("years", "Years", "YEARS", "years in medicare"),
  rate_ind        = c("individualrate"),
  practice_size   = c("practice_size"),
  patients        = c("medicare patients"),
  services        = c("services"),
  allowed_charges = c("allowed charges"),

  #YQM####
  year = c(
    "year",
    "Year",
    "YEAR",
    "perf_year",
    "Performance_Year",
    "PERF_YEAR",
    "fiscal_year",
    "payment_year",
    "Calendar Year",
    "Year(s) covered by the measure"
  ),
  month = c("month"),
  quarter = c("quarter"),

  #DATES####
  effective_date  = c("effective_date", "rateeffectivedate"),
  end_date        = c("end_date", "Fiscal Year End Date"),
  expiration_date = c("termination_date", "expiration_date", "rateexpirationdate"),
  process_date    = c("process_date"),
  start_date      = c(
    "start_date",
    "current_start_date",
    "initial_start_date",
    "Fiscal Year Begin Date",
    "implementation_date"
  ),
  work_date       = c("work_date", "WorkDate"),

  # ENUMERATED TYPES (FLAGS, CODES, INDICATORS)
  multi_npi    = c("MULTIPLE NPI FLAG"),
  plan_type    = c("plan_type"),
  owner_type   = c("Ownership_Type", "Ownership Type", "ownership_type"),
  esrd_network = c("ESRD_Network", "ESRD Network", "network"),
  chain        = c("Chain", "Chain Name", "chainnam"),
  modality     = c("Modality", "modal_f")
)

#' @autoglobal
#' @noRd
make_field_switch <- function(x) {
  e <- x |>
    purrr::map(\(x) cheapr::fast_df(field = x)) |>
    purrr::list_rbind(names_to = "constant") |>
    collapse::roworder(constant, field)

  function(x) {
    kit::vswitch(x, e$field, e$constant, default = clean_names(x), nThread = 4L)
  }
}

#' Field Switch
#'
#' Convert raw field names to a known constant.
#'
#' @param x A character vector of raw field names.
#'
#' @examples
#' field_switch(c("FIRST NAME - OWNER", "FIRST_NAME"))
#' field_switch("ASDFAGED sghfh")
#'
#' @returns A character vector of standardized field names.
#'
#' @autoglobal
#' @export
field_switch <- make_field_switch(x = FIELD)

#' Field Dictionary
#' @examplesIf interactive()
#' dictionary()
#' @returns A data frame
#' @autoglobal
#' @noRd
dictionary <- function() {
  get_pin("dicts")
}

#' Field Table
#' @examplesIf interactive()
#' field_table()
#' @returns A data frame
#' @autoglobal
#' @noRd
field_table <- function() {

  collapse::mtt(
    get_pin("field_types"),
    type = NULL,
    field = rm_nonascii(field),
    type = cheapr::case(
      field %in_% FIELD$first_name ~ "first_name",
      field %in_% FIELD$middle_name ~ "middle_name",
      field %in_% FIELD$last_name ~ "last_name",
      field %in_% FIELD$suffix ~ "suffix",
      field %in_% FIELD$org_name ~ "org_name",
      field %in_% FIELD$facility ~ "facility",
      field %in_% FIELD$dba_name ~ "dba_name",
      field %in_% FIELD$hospital_name ~ "hospital_name",
      field %in_% FIELD$brand_name ~ "brand_name",
      field %in_% FIELD$specialty ~ "specialty",
      field %in_% FIELD$specialty_code ~ "specialty_code",
      field %in_% FIELD$enid_ind ~ "enid_ind",
      field %in_% FIELD$enid_org ~ "enid_org",
      field %in_% FIELD$pac_ind ~ "pac_ind",
      field %in_% FIELD$pac_org ~ "pac_org",
      field %in_% FIELD$pac_own ~ "pac_own",
      field %in_% FIELD$hcpcs ~ "hcpcs",
      field %in_% FIELD$hcpcs_desc ~ "hcpcs_desc",
      field %in_% FIELD$npi ~ "npi",
      field %in_% FIELD$npi_prescriber ~ "npi_prescriber",
      field %in_% FIELD$npi_supply ~ "npi_supply",
      field %in_% FIELD$npi_render ~ "npi_render",
      field %in_% FIELD$npi_refer ~ "npi_refer",
      field %in_% FIELD$npi_seller ~ "npi_seller",
      field %in_% FIELD$npi_buyer ~ "npi_buyer",
      field %in_% FIELD$npi_entity ~ "npi_entity",
      field %in_% FIELD$multi_npi ~ "multi_npi",
      field %in_% FIELD$drg ~ "drg",
      field %in_% FIELD$drg_desc ~ "drg_desc",
      field %in_% FIELD$ccn ~ "ccn",
      field %in_% FIELD$ccn_alt ~ "ccn_alt",
      field %in_% FIELD$ccn_buy ~ "ccn_buyer",
      field %in_% FIELD$ccn_sell ~ "ccn_seller",
      field %in_% FIELD$ccn_render ~ "ccn_render",
      field %in_% FIELD$year ~ "year",
      field %in_% FIELD$years ~ "years",
      field %in_% FIELD$city ~ "city",
      field %in_% FIELD$city_own ~ "city_own",
      field %in_% FIELD$city_refer ~ "city_refer",
      field %in_% FIELD$city_render ~ "city_render",
      field %in_% FIELD$city_supply ~ "city_supply",
      field %in_% FIELD$city_scribe ~ "city_scribe",
      field %in_% FIELD$county ~ "county",
      field %in_% FIELD$country ~ "country",
      field %in_% FIELD$phone ~ "phone",
      field %in_% FIELD$zip ~ "zip",
      field %in_% FIELD$address ~ "address",
      field %in_% FIELD$address_2 ~ "address_2",
      field %in_% FIELD$state ~ "state",
      field %in_% FIELD$state_ind ~ "state_ind",
      field %in_% FIELD$state_org ~ "state_org",
      field %in_% FIELD$work_date ~ "work_date",
      field %in_% FIELD$start_date ~ "start_date",
      field %in_% FIELD$end_date ~ "end_date",

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

#' @autoglobal
#' @noRd
dict_field <- function() {
  field_table() |>
    collapse::slt(point, alias, year, field, type) |>
    collapse::sbt(!is.na(type) & !is.na(alias)) |>
    collapse::rsplit( ~ point, keep.by = FALSE) |>
    purrr::map(function(x)
      rm_all_na(x) |>
        fastplyr::f_nest_by(.by = "alias") |>
        fastplyr::f_ungroup()) |>
    purrr::list_rbind()
}

