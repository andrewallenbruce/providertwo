underscore  <- \(x) gsub("___owner$", "", x, perl = TRUE)

char_binary <- \(x) val_match(x, "N" ~ 0L, "Y" ~ 1L)

pct_prop    <- \(x) case(x == "0" ~ 0, is_na(x) ~ NA_real_, .default = as.double(x) / 100)

rhc_enroll <- careGroup("RHC")@members |>
  _[["RHC_enrollments"]] |>
  prop("identifier") |>
  quick_care() |>
  mtt(incorporation_date = providertwo:::as_date(incorporation_date),
      multiple_npi_flag = char_binary(multiple_npi_flag),
      address = cheapr_if_else(
        cheapr::is_na(address_line_2),
        address_line_1,
        paste(address_line_1, address_line_2))) |>
  slt(
    npi,
    has_mult_npi = multiple_npi_flag,
    ccn,
    pac = associate_id,
    enid = enrollment_id,
    enid_state = enrollment_state,
    org_name = organization_name,
    dba_name = doing_business_as_name,
    inc_date = incorporation_date,
    inc_state = incorporation_state,
    org_type = organization_type_structure,
    org_type_2 = organization_other_type_text,
    proprietary_nonprofit,
    address,
    city,
    state,
    zip = zip_code)

rhc_enroll |>
  purse()
