source(here::here("data-raw", "pins_internal.R"))
source(here::here("data-raw", "field_fns.R"))

the$clog$caid$current |>
  alias_column(end_caid$current) |>
  sbt(is.na(alias))

prov_fld <- fields_current(
  the$clog$prov$current,
  end_prov$current)

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

caid_fld <- fields_current(
  sbt(the$clog$caid$current,
      title %!in_% c(
        "Monthly Enrollment - Test",
        "NAM CAHPS 2014 Public Use",
        "PI dataset",
        "State Medicaid and CHIP Applications, Eligibility Determinations, and Enrollment Data",
        "State Medicaid and CHIP Test",
        "Test07232025-enrollment"
        )
      ),
  end_caid$current)

caid_fld <- caid_fld |>
  collapse::mtt(catalog = "caid", point = "current")

hgov_fld <- fields_current(the$clog$hgov$current, end_hgov$current)

title <- sbt(the$clog$care$temporal, gdetect(title, "Quality Payment")) |> _$title

orig <- sbt(the$clog$care$temporal, gdetect(title, "Quality Payment")) |>
  _$endpoints |>
  yank() |>
  mtt(title = title) |>
  slt(year, title, modified, identifier)

x <- orig |> get_elem("^year$|^identifier$", regex = TRUE)

url <- set_names(x$identifier, x$year)


qpp_fields <- purrr::imap(url, function(x, i) {
  fastplyr::new_tbl(
    year  = i,
    field = x |>
      httr2::request() |>
      httr2::req_error(is_error = ~ FALSE) |>
      httr2::req_perform() |>
      httr2::resp_body_json(simplifyVector = TRUE, check_type = FALSE) |>
      names2()
  ) |>
    collapse::mtt(title = title)
})

qpp_fields <- qpp_fields |>
  purrr::list_rbind() |>
  collapse::mtt(catalog = "care", point = "temporal") |>
  collapse::join(orig) |>
  collapse::slt(catalog, point, year, field, title, modified)

qpp_fields |>
  # sbt(stringi::stri_detect_regex(field, "city", case_insensitive = TRUE)) |>
  fcount(field) |>
  roworder(field) |>
  print(n = 200)

build(endpoint("quality_payment"), query(npi = c(1144544834, 1043477615, 1932365699, 1225701881)))

care_tmp <- fields_temporal_care(the$clog$care$temporal, end_care$temporal)
caid_tmp <- fields_temporal(the$clog$caid$temporal, end_caid$temporal)
hgov_tmp <- fields_temporal(the$clog$hgov$temporal, end_hgov$temporal)
open_tmp <- fields_temporal(the$clog$open$temporal, end_open$temporal)

field_types <- vctrs::vec_rbind(
  get_pin("field_types"),
  caid_tmp,
  hgov_tmp,
  open_tmp)

pin_update(
  field_tbl,
  name = "field_types",
  title = "Field Typing",
  description = "Field Typing")
