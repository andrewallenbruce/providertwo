source(here::here("data-raw", "pins_internal.R"))

fields_current <- function(catalog_tbl, alias_list) {

  mirai::daemons(0)

  e <- rlang::enquo(catalog_tbl) |>
    rlang::as_label() |>
    strsplit("[$]", perl = TRUE) |>
    yank()

  x <- collapse::slt(catalog_tbl, title, modified, identifier) |>
    collapse::roworder(title, -modified)

  url_list <- rlang::set_names(x[["identifier"]], x[["title"]])

  # res <- base_url |>
  #   map(request) |>
  #   req_perform_parallel(on_error = "continue") |>
  #   map2(c("/dataset", rep(NA_character_, 4)), function(x, q) {
  #     resp_body_string(x) |>
  #       fparse(query = if (is.na(q)) NULL else q) |>
  #       as_fibble()
  #   }) |>
  #   set_names(names(base_url))

  mirai::daemons(6)

  res <- purrr::imap(url_list, purrr::in_parallel(\(x, i) {
    fastplyr::new_tbl(
      title = i,
      field = x |>
        httr2::request() |>
        httr2::req_error(is_error = ~ FALSE) |>
        httr2::req_perform() |>
        httr2::resp_body_json(simplifyVector = TRUE, check_type = FALSE) |>
        _$query |>
        _$properties
    )
  }))

  mirai::daemons(0)

  empty <- res |>
    purrr::keep(vctrs::vec_is_empty) |>
    names() |>
    fastplyr::f_enframe(value = "title")

  non_empty <- res |>
    purrr::discard(vctrs::vec_is_empty) |>
    purrr::list_rbind()

  vctrs::vec_rbind(non_empty, empty) |>
    collapse::mtt(catalog = e[3], point = e[4]) |>
    join_on_title(x) |>
    alias_column(alias_list) |>
    collapse::slt(catalog, point, alias, field, title, modified)
}

prov_fld <- fields_current(the$clog$prov$current, end_prov$current)

prov_fld |>
  fcount(alias, decreasing = TRUE, sort = TRUE) |>
  roworder(-N) |>
  print(n = Inf)

prov_fld |>
  field_type_col() |>
  fcount(type, decreasing = TRUE, sort = TRUE) |>
  roworder(-N) |>
  print(n = 200)

prov_fld |>
  field_type_col() |>
  sbt(is.na(type)) |>
  # fcount(type, decreasing = TRUE, sort = TRUE) |>
  # sbt(gdetect(field, "year")) |>
  fcount(field, decreasing = TRUE, sort = TRUE) |>
  roworder(-N) |>
  print(n = 200)

fields_current_care <- function(catalog_tbl, alias_list) {

  mirai::daemons(0)

  e <- rlang::enquo(catalog_tbl) |>
    rlang::as_label() |>
    strsplit("[$]", perl = TRUE) |>
    yank()

  x <- collapse::slt(catalog_tbl, title, modified, identifier) |>
    collapse::roworder(title, -modified)

  url_list <- rlang::set_names(x[["identifier"]], x[["title"]])

  mirai::daemons(6)

  res <- purrr::imap(url_list, purrr::in_parallel(\(x, i) {
    fastplyr::new_tbl(
      title = i,
      field = x |>
        httr2::request() |>
        httr2::req_error(is_error = ~ FALSE) |>
        httr2::req_perform() |>
        httr2::resp_body_json(simplifyVector = TRUE, check_type = FALSE) |>
        collapse::get_elem("meta") |>
        collapse::get_elem("headers")
    )
  }))

  mirai::daemons(0)

  empty <- res |>
    purrr::keep(vctrs::vec_is_empty) |>
    names() |>
    fastplyr::f_enframe(value = "title")

  non_empty <- res |>
    purrr::discard(vctrs::vec_is_empty) |>
    purrr::list_rbind()

  vctrs::vec_rbind(non_empty, empty) |>
    collapse::mtt(catalog = e[3], point = e[4]) |>
    join_on_title(x) |>
    alias_column(alias_list) |>
    collapse::slt(catalog, point, alias, field, title, modified) |>
    collapse::sbt(stringi::stri_detect_regex(title, "CMS Program Statistics", negate = TRUE))
}

care_fld <- fields_current_care(the$clog$care$current, end_care$current)

care_fld |>
  alias_after(end_care$temporal) |>
  fcount(alias, decreasing = TRUE, sort = TRUE) |>
  roworder(-N) |>
  print(n = Inf)

care_fld |>
  # alias_after(end_care$temporal) |>
  sbt(stringi::stri_detect_regex(field, "ccn", case_insensitive = TRUE)) |>
  # sbt(stringi::stri_detect_regex(field, "years", case_insensitive = TRUE, negate = TRUE)) |>
  fcount(field, decreasing = TRUE, sort = TRUE) |>
  roworder(-N) |>
  print(n = 50)

care_fld |>
  field_type_col() |>
  slt(catalog, alias, field, type) |>
  fcount(type, decreasing = TRUE, sort = TRUE) |>
  roworder(-N) |>
  print(n = Inf)

open_fld <- fields_current(the$clog$open$current, end_open$current)

open_fld |>
  fcount(alias, decreasing = TRUE, sort = TRUE) |>
  roworder(-N) |>
  print(n = Inf)

open_fld |>
  sbt(stringi::stri_detect_regex(field, "middle_name", case_insensitive = TRUE)) |>
  # sbt(stringi::stri_detect_regex(field, "years", case_insensitive = TRUE, negate = TRUE)) |>
  fcount(field, decreasing = TRUE, sort = TRUE) |>
  roworder(-N) |>
  print(n = 50)

open_fld |>
  field_type_col() |>
  slt(catalog, alias, field, type) |>
  fcount(type, decreasing = TRUE, sort = TRUE) |>
  roworder(-N) |>
  print(n = Inf)

pin_update(
  vctrs::vec_rbind(prov_fld, care_fld, open_fld),
  name = "field_types",
  title = "Field Typing",
  description = "Field Typing")

all <- readr::read_csv(fs::dir_ls("data-raw/fields")) |>
  mtt(
    modified = as_date(modified),
    catalog = val_match(
      catalog,
      "Medicare" ~ "care",
      "Medicaid" ~ "caid",
      "Provider" ~ "prov",
      "OpenPay" ~ "open",
      "HealthcareGov" ~ "hgov",
      .default = catalog
    ),
    type = tolower(field),
    category = case(
      gdetect(type, "date|year|month") ~ "date",
      gdetect(type, "fips") ~ "fips",
      gdetect(type, "address") ~ "address",
      gdetect(type, "state|state_name") ~ "state",
      gdetect(type, "zip") ~ "zip",
      gdetect(type, "city|citytown") ~ "city",
      gdetect(type, "county|countyparish|county_name") ~ "county",
      gdetect(type, "ruca") ~ "ruca",
      gdetect(type, "ccn") ~ "ccn",
      gdetect(type, "npi") ~ "npi",
      gdetect(type, "hcpcs") ~ "hcpcs",
      gdetect(type, "ndc") ~ "ndc",
      gdetect(type, "associate id|associate id - owner") ~ "pac",
      gdetect(type, "enrollment id") ~ "enid",
      gdetect(type, "phone|telephone") ~ "phone",
      gdetect(type, "email") ~ "email",
      gdetect(type, "doing business as name|organization_name|organization name|first name|middle name|last name|first_name|middle_name|last_name|suff|provider name|provider_name|aco_exec_name|aco_public_name|aco_medical_director_name|aco_compliance_contact_name") ~ "name",
      .default = NA_character_
    )
  ) |>
  slt(catalog, field, type, category, title, modified)

all |>
  fcount(category) |>
  roworder(-N) |>
  print(n = 300)

readr::write_csv(all, "data-raw/fields/all_fields.csv")

obj <- endpoint("quality_payment")

obj@dimensions@fields

field_types <- readr::read_csv(
  file       = fs::path_abs("data-raw/fields/all_fields.csv"),
  col_types  = readr::cols(
    catalog  = readr::col_character(),
    field    = readr::col_character(),
    type     = readr::col_character(),
    category = readr::col_character(),
    title    = readr::col_character(),
    modified = readr::col_date(format = "")
  ))

ftype <- field_types |>
  sbt(catalog == "care" & title == "Quality Payment Program Experience" & !is.na(category)) |>
  slt(field, category) |>
  funique()

ftype_list <- set_names(as.list(ftype$category), ftype$field)

obj@dimensions@fields <- list_modify(obj@dimensions@fields, ftype_list)
obj@dimensions


# x <- endpoint("pdc_affiliations")
# x@dimensions@fields |>
# list_modify(prov_fields("affiliations"))
#' @autoglobal
#' @noRd
prov_fields <- function(x) {
  switch(
    x,
    affiliations = list(
      npi                  = "npi",
      ind_pac_id           = "pac_ind",
      provider_last_name   = "last_name",
      provider_first_name  = "first_name",
      provider_middle_name = "middle_name",
      suff                 = "suffix_name",
      facility_type        = factor(
        c(
          "Hospital",
          "Long-term care hospital",
          "Nursing home",
          "Inpatient rehabilitation facility",
          "Home health agency",
          "Hospice",
          "Skilled nursing facility",
          "Dialysis facility"
        )
      ),
      facility_affiliations_certification_number = "ccn",
      facility_type_certification_number = "ccn"
    ),
    clinicians = list(
      npi = list(type = "npi", form = "integer(10)"),
      ind_pac_id = list(type = "pac_ind", form = "character(10)"),
      ind_enrl_id = list(type = "enid_ind", form = "character(15)"),
      provider_last_name = list(type = "last_name", form = "character(0)"),
      provider_first_name = list(type = "first_name", form = "character(0)"),
      provider_middle_name = list(type = "middle_name", form = "character(0)"),
      suff = list(type = "suffix_name", form = "character(0)"),
      gndr = list(type = "gender", form = factor(c("F", "M"))),
      cred = list(type = "credentials", form = "character(0)"),
      med_sch = character(0),
      grd_yr = list(type = "year", form = "integer(4)"),
      pri_spec = list(type = "specialty", form = "character(0)"),
      sec_spec_1 = list(type = "specialty", form = "character(0)"),
      sec_spec_2 = list(type = "specialty", form = "character(0)"),
      sec_spec_3 = list(type = "specialty", form = "character(0)"),
      sec_spec_4 = list(type = "specialty", form = "character(0)"),
      sec_spec_all = list(type = "specialty", form = "character(0)"),
      telehlth = list(type = "enum", form = factor(c("Y", "M", "N"))),
      facility_name = list(type = "facility_name", form = "character(0)"),
      org_pac_id = list(type = "pac_org", form = "character(10)"),
      num_org_mem = list(type = "count", form = "integer(0)"),
      adr_ln_1 = list(type = "address_1", form = "character(0)"),
      adr_ln_2 = list(type = "address_2", form = "character(0)"),
      ln_2_sprs = logical(0),
      citytown = list(type = "city", form = "character(0)"),
      state = list(type = "state", form = "character(2)"),
      zip_code = list(type = "zip", form = "character(9)"),
      telephone_number = list(type = "phone", form = "integer(10)"),
      ind_assgn = list(type = "enum", form = factor(c("Y", "M", "N"))),
      grp_assgn = list(type = "enum", form = factor(c("Y", "M", "N"))),
      adrs_id = character(0)
    ),
    utilization = list(
      npi = "npi",
      ind_pac_id = "pac_ind",
      provider_last_name = "last_name",
      provider_first_name = "first_name",
      provider_middle_name = "middle_name",
      suff = "suffix_name",
      procedure_category = character(0),
      count = integer(0),
      percentile = integer(0),
      profile_display_indicator = character(0)
    )
  )
}
