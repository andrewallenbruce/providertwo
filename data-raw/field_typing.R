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

the$clog$caid$current |>
  alias_column(end_caid$current) |>
  sbt(is.na(alias))

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

#   year      N
# 1 2023    204
# 2 2022    165
# 3 2021     92
# 4 2020     92
# 5 2019     92
# 6 2018     92
# 7 2017     92

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

get_pin("field_types") |>
  field_type_col()

build(endpoint("quality_payment"), query(npi = c(1144544834, 1043477615, 1932365699, 1225701881)))

pin_update(
  # vctrs::vec_rbind(prov_fld, care_fld, open_fld, caid_fld, hgov_fld, qpp_fields),
  field_types,
  name = "field_types",
  title = "Field Typing",
  description = "Field Typing")
