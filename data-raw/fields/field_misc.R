field_tbl <- field_table() |>
  mtt(
    field = ifelse(is.na(field), "NO_FIELDS", field),
    field = ifelse(field == "NO_FIELDS", "_NO_FIELD_", field)
  ) |>
  sbt(field == "_NO_FIELD_") |>
  print(n = Inf)

field_table() |>
  # sbt(is.na(type)) |>
  sbt(
    # catalog == "hgov" &
    grep("^[N]", field, perl = TRUE)) |>
  fcount(field) |>
  roworder(field) |>
  print(n = 50) |>
  sbt(N >= 10 & N <= 30) |>
  print(n = Inf)

fields_exact$first_name

#' @autoglobal
#' @noRd
reduce_fields <- function(x) {
  flist(
    f_union = purrr::reduce(x$headers, vctrs::vec_set_union) |>
      kit::psort(nThread = 4L),
    f_common = purrr::reduce(x$headers, vctrs::vec_set_intersect) |>
      kit::psort(nThread = 4L),
    f_unique = purrr::imap(x$headers, function(x, i)
      vctrs::vec_set_difference(x, y = f_common)) |>
      purrr::map(\(x) kit::psort(x, nThread = 4L))
  )
}

#' @noRd
#' @autoglobal
class_schema <- S7::new_class(
  name         = "class_schema",
  package      = NULL,
  properties   = list(
    alias = S7::class_character,
    columns = S7::class_list
  ) # priority/only most common fields?
)

#' @autoglobal
#' @noRd
enumerate <- function(x) {
  glue::glue("c({toString(glue::single_quote(x))})")
}

#' @autoglobal
#' @noRd
aff_fields <- function() {
  facility_types <- c(
    "Hospital",
    "Long-term care hospital",
    "Nursing home",
    "Inpatient rehabilitation facility",
    "Home health agency",
    "Hospice",
    "Skilled nursing facility",
    "Dialysis facility"
  )

  fastplyr::new_tbl(
    alias = "pdc_affiliation",
    field = c(
      "npi",
      "ind_pac_id",
      "provider_last_name",
      "provider_first_name",
      "provider_middle_name",
      "suff",
      "facility_type",
      "facility_affiliations_certification_number",
      "facility_type_certification_number"
    ),
    param = c(
      "npi",
      "pac_ind",
      "last_name",
      "first_name",
      "middle_name",
      "suffix",
      "facility_type",
      "ccn_unit",
      "ccn_primary"
    ),
    constant = c(
      "npi",
      "pac",
      "name",
      "name",
      "name",
      "name",
      "enum",
      "ccn",
      "ccn"
    ),
    subtype = c( # tag
      "individual",
      "individual",
      "individual",
      "individual",
      "individual",
      "individual",
      "individual",
      "numeric",
      "character"
    ),
    choices = c(
      rep(NA_character_, 6),
      enumerate(facility_types),
      rep(NA_character_, 2)
    ),
    definition = c(
      "Unique clinician ID assigned by NPPES",
      "Unique individual clinician ID assigned by PECOS",
      "Individual clinician last name",
      "Individual clinician first name",
      "Individual clinician middle name",
      "Individual clinician suffix",
      "Facilities can fall into the following type categories: Hospitals, Long-term Care Hospital, Nursing Home, Inpatient Rehabilitation Facility, Home Health Agency, Hospice and Dialysis Facility",
      "Medicare CCN of facility type or unit within hospital where an individual clinician provides service",
      "Medicare CCN of the primary hospital where individual clinician provides service, should the clinician provide services in a unit within the hospital"
    )
  )
}

#' @autoglobal
#' @noRd
util_fields <- function() {

  # TODO may not be all procedures
  procedure <- c(
    "Cataract surgery",
    "Colonoscopy",
    "Coronary angioplasty and stenting",
    "Coronary artery bypass graft (CABG)",
    "Hernia repair (minimally invasive)",
    "Hernia repair - groin (open)",
    "Hip replacement",
    "Knee replacement",
    "Laminectomy or laminotomy (partial removal of spine bones)",
    "Leg revascularization (restoring blood flow)",
    "Lower limb (leg) arthroscopy (minimally invasive joint repair)",
    "Mastectomy",
    "Melanoma (skin cancer) excision",
    "Pacemaker insertion or repair",
    "Prostate resection",
    "Spinal fusion",
    "Upper gastrointestinal (GI) endoscopy for acid reflux",
    "Upper limb (arm) arthroscopy (minimally invasive joint repair)",
    "Varicose vein removal"
  )

  # TODO count is ints
  # except for less than 11
  # which is "1-10"
  # ifelse(count < 11, "1-10", count)

  fastplyr::new_tbl(
    alias = "pdc_utilization",
    field = c(
      "npi",
      "ind_pac_id",
      "provider_last_name",
      "provider_first_name",
      "provider_middle_name",
      "suff",
      "procedure_category",
      "count",
      "percentile",
      "profile_display_indicator"
    ),
    param = c(
      "npi",
      "pac_ind",
      "last_name",
      "first_name",
      "middle_name",
      "suffix",
      "procedure",
      "count",
      "percentile",
      "profile_display"
    ),
    constant = c(
      "npi",
      "pac",
      "name",
      "name",
      "name",
      "name",
      "enum",
      NA_character_,
      NA_character_,
      "enum"
    ),
    subtype = c(
      "individual",
      "individual",
      "individual",
      "individual",
      "individual",
      "individual",
      "character",
      "integer",
      "integer",
      "character"
    ),
    choices = c(
      rep(NA_character_, 6),
      enumerate(procedure),
      glue::glue("ifelse(count < 11, '1-10', count)"),
      NA_character_,
      enumerate(c("Y", "N"))
    )
  )
}

#' @autoglobal
#' @noRd
clin_fields <- function() {

  ymn    <- enumerate(c("Y", "M", "N"))
  gender <- enumerate(c("F", "M"))

  fastplyr::new_tbl(
    alias = "pdc_clinician",
    field = c(
      "npi",
      "ind_pac_id",
      "ind_enrl_id",
      "provider_last_name",
      "provider_first_name",
      "provider_middle_name",
      "suff",
      "gndr",
      "cred",
      "med_sch",
      "grd_yr",
      "pri_spec",
      "sec_spec_1",
      "sec_spec_2",
      "sec_spec_3",
      "sec_spec_4",
      "sec_spec_all",
      "telehlth",
      "facility_name",
      "org_pac_id",
      "num_org_mem",
      "adr_ln_1",
      "adr_ln_2",
      "ln_2_sprs",
      "citytown",
      "state",
      "zip_code",
      "telephone_number",
      "ind_assgn",
      "grp_assgn",
      "adrs_id"
    ),
    param = c(
      "npi",
      "pac_ind",
      "enid_ind",
      "last_name",
      "first_name",
      "middle_name",
      "suffix",
      "gender",
      "credentials",
      "school_name",
      "grad_year",
      "specialty_primary",
      "sec_spec_1",
      "sec_spec_2",
      "sec_spec_3",
      "sec_spec_4",
      "specialty_other",
      "telehealth",
      "facility_name",
      "pac_org",
      "count_org",
      "address_1",
      "address_2",
      "add_press",
      "city",
      "state",
      "zip",
      "phone",
      "ind_assign",
      "group_assign",
      "id_add"
    ),
    constant = c(
      "npi",
      "pac",
      "enid",
      "name", # last
      "name", # first
      "name", # middle
      "name", # suffix
      "enum", # gender
      NA_character_, # credentials
      "name", # med school
      "year", # grad year
      "enum", # specialty primary
      rep(NA_character_, 4), # sec spec 1
      "enum", # specialty secondary
      "enum", # telehealth
      "name", # facility name
      "pac", # org pac
      NA_character_, # member count
      "address", # address 1
      "address", # address 2
      "enum", # address 2 suppressed
      "city",
      "state",
      "zip",
      "phone",
      "enum", # individual accepts assignment
      "enum", # group accepts assignment
      NA_character_ # address id
    ),
    subtype = c(
      "individual",
      "individual",
      "individual",
      "individual",
      "individual",
      "individual",
      "individual",
      "individual",
      "individual",
      "individual",
      "individual",
      "individual",
      NA_character_, # sec spec 1
      NA_character_,
      NA_character_,
      NA_character_,
      "individual",
      "organization",
      "organization",
      "organization",
      "organization",
      "organization",
      "organization",
      "organization",
      "organization",
      "organization",
      "organization",
      "organization",
      "individual",
      "organization",
      "organization"
    ),
    choices = c(
      rep(NA_character_, 7),
      gender,
      rep(NA_character_, 3),
      ymn, # TODO specialty primary
      rep(NA_character_, 4),
      ymn, # TODO specialty secondary
      ymn,
      rep(NA_character_, 5),
      ymn,
      rep(NA_character_, 4),
      ymn,
      ymn,
      NA_character_
    )
  )
}

#' @autoglobal
#' @noRd
pdc_fields <- function() {
  collapse::rowbind(fill = TRUE, aff_fields(), util_fields(), clin_fields())
}

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
