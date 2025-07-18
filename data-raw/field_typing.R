clog <- catalogs()

# care_fields <- clog$care$current |>
#   slt(title, modified, identifier)
#
x <- clog$hgov$current |> slt(title, modified, identifier) |> roworder(-modified)
# e <- x$endpoints |> map(\(x) slt(x, modified, identifier)) |> roworder(-modified) |> _[1, ])
# n <- cheapr::cheapr_rep_each(x$title, list_lengths(e))

# care_fields <- cheapr::col_c(
#   title = n,
#   e |> purrr::list_rbind()) |>
#   fastplyr::as_tbl()


pfields <- set_names(x[["identifier"]], x[["title"]])

fields_tbl9 <- purrr::imap(pfields, function(x, i) {
  fastplyr::new_tbl(
    title = i,
    field = x |>
      httr2::request() |>
      httr2::req_error(is_error = ~ FALSE) |>
      perform_simple() |>
      # names()
      # _$meta |>
      # _$headers
      _$query |>
      _$properties
  )
}) |>
  purrr::list_rbind() |>
  mtt(catalog = "HealthcareGov") |>
  join_on_title(x) |>
  slt(catalog, title, field, modified)


readr::write_csv(fields_tbl9, "data-raw/fields/hgov_curr_fields.csv")

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
