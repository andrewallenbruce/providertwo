#' @autoglobal
#' @noRd
fields_exact <- list(
  # NAME
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
    "provider_name",
    "recipient_name",
    "PRVDR_NAME",
    "Prscrbr_Last_Org_Name",
    "Rfrg_Prvdr_Last_Name_Org"
  ),
  suffix = c("suff"),
  facility = c(
    "FAC_NAME",
    "facility_name"
    ),
  measure = c("measure_name"),
  issuer = c("issuer_name"),
  dba_name = c(
    "DOING BUSINESS AS NAME",
    "Group Legal Business Name",
    "DOING BUSINESS AS NAME - OWNER"
    ),
  hospital_name = c("Hosp_Name"),
  aco_name = c("ACO_NAME", "aco_name"),
  brand_name = c("Brand Name", "Brnd_Name"),

  # SPECIALTY
  specialty_code = c(
    "PROVIDER_TYPE_CD",
    "PROVIDER TYPE CODE"
    ),
  specialty = c(
    "Specialty",
    "PROVIDER_TYPE_DESC",
    "PROVIDER TYPE TEXT",
    "Provider Type Text",
    "Individual Specialty Description"
  ),

  # IDENTIFIERS
  npi = c(
    "npi",
    "NPI",
    "Individual NPI",
    "covered_recipient_npi",
    "National Provider Identifier"
  ),
  npi_entity      = c("entity_npi"),
  npi_buyer       = c("NPI - BUYER"),
  npi_seller      = c("NPI - SELLER"),
  npi_refer       = c("Rfrg_NPI"),
  npi_render      = c("Rndrng_NPI"),
  npi_supply      = c("Suplr_NPI"),
  npi_prescriber  = c("Prscrbr_NPI", "PRSCRBR_NPI"),
  multi_npi       = c("MULTIPLE NPI FLAG"), # FLAG

  # Enrollment ID
  enid_ind = c(
    "ENRLMT_ID",
    "ENROLLMENT ID",
    "Enrollment ID",
    "Individual Enrollment ID"
    ),
  enid_org = c(
    "Group Enrollment ID"
    ),

  pac_ind = c(
    "PECOS_ASCT_CNTL_ID",
    "ind_pac_id",
    "ASSOCIATE ID",
    "Individual PAC ID"
    ),
  pac_org = c(
    "org_pac_id",
    "Group PAC ID"
    ),
  pac_own = c("ASSOCIATE ID - OWNER"),

  ccn = c(
    "PRVDR_NUM",
    "alternate_ccn",
    "Alternate_CCNs",
    "ccn",
    "CCN",
    "Provider CCN",
    "cms_certification_number_ccn",
    "CAH OR HOSPITAL CCN",
    "HHA-based Hospice Provider CCN"
  ),
  ccn_alt = c("alternate_ccn", "Alternate_CCNs"),
  ccn_buy = c("CCN - BUYER"),
  ccn_sell = c("CCN - SELLER"),
  ccn_render = c("Rndrng_Prvdr_CCN"),

  # TERMINOLOGY
  hcpcs = c(
    "hcpcs_cd",
    "HCPCS_Cd",
    "HCPCS_CD"),
  hcpcs_desc = c(
    "HCPCS_Desc",
    "HCPCS_DESC"
    ),

  # LOCATION
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
    "Geographic Location City Name"
  ),
  city_own = c("CITY - OWNER"),
  city_org = c("PRVDR_CITY"),
  city_refer = c("Rfrg_Prvdr_City"),
  city_render = c("Rndrng_Prvdr_City"),
  city_supply = c("Suplr_Prvdr_City"),
  city_scribe = c("Prscrbr_City"),

  county = c(
    "county",
    "County",
    "county_name",
    "COUNTY_NAME",
    "County_Name",
    "County Name",
    "State County Name",
    "countyparish"
  ),
  country = c(
    "country",
    "country_name",
    "Rfrg_Prvdr_Cntry"
    ),

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
  address_2 = c(
    "address_line_2",
    "adr_ln_2",
    "Second Line Street Address",
    "ADDRESS LINE 2"
    ),

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
  state_ind = c(
    "Individual State Code",
    "Enrollment State Code"
    ),

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

  # COUNTS (NUMERIC FIELDS)
  years = c(
    "years",
    "Years",
    "YEARS",
    "years in medicare"
  ),
  individual_rate = c("individualrate"),

  # DATE YYYY
  year = c(
    "year",
    "Year",
    "YEAR",
    "perf_year",
    "Performance_Year",
    "PERF_YEAR",
    "fiscal_year",
    "payment_year",
    "Calendar Year"
  ),
  month = c("month"),
  quarter = c("quarter"),
  work_date = c("work_date"),
  start_date = c(
    "start_date",
    "current_start_date",
    "initial_start_date",
    "Fiscal Year Begin Date"
    ),
  end_date = c(
    "end_date",
    "Fiscal Year End Date"
  ),
  effective_date = c(
    "effective_date",
    "rateeffectivedate"
    ),
  expiration_date = c(
    "termination_date",
    "expiration_date",
    "rateexpirationdate"
    ),
  process_date = c("process_date"),

  # ENUMERATED TYPES (FLAGS, CODES, INDICATORS)
  plan_type = c("plan_type")
)
