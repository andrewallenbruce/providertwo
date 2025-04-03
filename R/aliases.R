#' @examplesIf rlang::is_interactive()
#' care_main("enrollees")
#' care_main("opt_out")
#' care_main("order_refer")
#' care_main("reassignments")
#' care_main("hospitals")
#' care_main("laboratories")
#' care_main("crosswalk")
#' care_main("rbcs")
#' care_main("facilities")
#' care_main("home_health")
#' care_main("hospice")
#' care_main("dialysis")
#' care_main("skilled_nursing")
#' @autoglobal
#' @noRd
care_main <- function(x, call = caller_env()) {

  x <- nswitch(
    x,
    "enrollees",             "Public Provider Enrollment",
    "opt_out",               "Opt Out Affidavits",
    "order_refer",           "Order and Referring",
    "reassignments",         "Revalidation Reassignment List",
    "hospitals",             "Hospital Enrollments",
    "laboratories",          "Provider of Services File - Clinical Laboratories",
    "facilities",            "Provider of Services File - Hospital & Non-Hospital Facilities",
    "crosswalk",             "Medicare Provider and Supplier Taxonomy Crosswalk",
    "rbcs",                  "Restructured BETOS Classification System",
    "home_health",           "Home Health Agency Enrollments",
    "hospice",               "Hospice Enrollments",
    "dialysis",              "Medicare Dialysis Facilities",
    "skilled_nursing",       "Skilled Nursing Facility Enrollments",
    default = NA_character_,
    nThread = 4L
  )

  if (na(x)) cli_abort("x" = "No matches found.", call = call)

  if (!exists("catalog")) catalog <- catalogs()

  select_alias(catalog$main$current, x) |> c()
}

#' @examplesIf rlang::is_interactive()
#' care_group("hospitals")
#' care_group("rhc")
#' care_group("fqhc")
#' care_group("pending")
#' @autoglobal
#' @noRd
care_group <- function(x, call = caller_env()) {

  x <- nswitch(
    x,
    "hospitals", "^Hospital",
    "rhc",       "Rural Health Clinic",
    "fqhc",      "Federally Qualified Health Center",
    "pending",   "Pending Initial Logging and Tracking",
    default = NA_character_,
    nThread = 4L
  )

  if (na(x)) cli_abort("x" = "No matches found.", call = call)

  if (!exists("catalog")) catalog <- catalogs()

  select_alias(catalog$main$current, x) |> slt(-filetype)
}

#' @examplesIf rlang::is_interactive()
#' care_temp("quality_payment")
#' @autoglobal
#' @noRd
care_temp <- function(x) {

  x <- nswitch(
    x,
    "quality_payment",  "Quality Payment Program Experience",
    default = NA_character_,
    nThread = 4L
  )

  if (na(x)) cli_abort("x" = "No matches found.", call = call)

  if (!exists("catalog")) catalog <- catalogs()

  select_alias(catalog$main$temporal, x)

}

#' @examplesIf rlang::is_interactive()
#' care_temp_group("utilization")
#' care_temp_group("prescribers")
#' care_temp_group("suppliers")
#' care_temp_group("outpatient")
#' care_temp_group("inpatient")
#' @autoglobal
#' @noRd
care_temp_group <- function(x) {

  x <- nswitch(
    x,
    "prescribers", "Medicare Part D Prescribers",
    "utilization", "Medicare Physician & Other Practitioners",
    "outpatient",  "Medicare Outpatient Hospitals",
    "inpatient",   "Medicare Inpatient Hospitals",
    "suppliers",   "Medicare Durable Medical Equipment, Devices & Supplies",
    default = NA_character_,
    nThread = 4L
  )

  if (na(x)) cli_abort("x" = "No matches found.", call = call)

  if (!exists("catalog")) catalog <- catalogs()

  x <- select_alias(catalog$main$temporal, x) |>
    mtt(group = clean_names(stri_extract_first_regex(title, "(?<=-\\s).*$")),
        title = stri_extract_first_regex(title, "^.*(?=\\s-)")) |>
    colorder(title, group)

  x2 <- map2(x$data,  x$group, function(x, y)
             mtt(x, group = clean_names(y)) |>
               slt(year, group, identifier, resources)) |>
    rowbind() |>
    join(slt(x, -data, -title, -periodicity, -contact),
         on = "group",
         verbose = 0)

  years  <- funique(x2$year)
  groups <- funique(x2$group)

  q <- map(sbt(x2, year == fmax(year)) |>
             _[["identifier"]], \(x)
           dims_main_temp(x)) |>
    set_names(groups)

  x2 <- rsplit(x2, ~ group)

  template <- glue(
    "
  {group} = list(
  description = x2${group}$description[1],
  dictionary  = x2${group}$dictionary[1],
  site        = x2${group}$site[1],
  rows        = q${group}$rows,
  pages       = q${group}$pages,
  fields      = q${group}$fields,
  endpoints   = slt(x2${group}, -description, -dictionary, -site))
  ",
    group = x$group) |>
    glue_collapse(sep = ",\n")

  glue(
    "
  list(
    title       = x$title[1],
    periodicity = x$periodicity[1],
    contact     = x$contact[1],
    years       = years,
    groups      = groups,
  {template}
  )
  "
  ) |>
    parse_expr() |>
    eval_bare()

}

#' @examplesIf rlang::is_interactive()
#' pro_main("affiliations")
#' pro_main("clinicians")
#' pro_main("utilization")
#' @autoglobal
#' @noRd
pro_main <- function(x) {

  x <- nswitch(
    x,
    "affiliations",  "^Facility Affiliation Data$",
    "clinicians",    "^National Downloadable File$",
    "utilization",   "^Utilization Data$",
    default = NA_character_,
    nThread = 4L
  )

  if (na(x)) cli_abort("x" = "No matches found.", call = call)

  if (!exists("catalog")) catalog <- catalogs()

  select_alias(catalog$pro, x)

}

#' @examplesIf rlang::is_interactive()
#' pro_group("mips")
#' @autoglobal
#' @noRd
pro_group <- function(x) {

  x <- nswitch(
    x,
    "mips", "^PY 2022",
    default = NA_character_,
    nThread = 4L)

  if (na(x)) cli_abort("x" = "No matches found.", call = call)

  if (!exists("catalog")) catalog <- catalogs()

  select_alias(catalog$pro, x)
}

#' @autoglobal
#' @noRd
alias_open <- function(x) {
  nswitch(
    x,
    "prof_cov",        "^Covered Recipient Profile Supplement",
    "prof_phys",       "^Physician \\(Distinct\\) Profile Information",
    "prof_info",       "^Profile Information",
    "prof_map",        "^Provider Profile ID Mapping Table",
    "prof_entity",     "^Reporting Entity Profile Information",
    "prof_teach",      "^Teaching Hospital Profile Information",
    "dashboard",       "^Summary Dashboard",
    "pay_state_total", "^State Level Payment Total and Averages for all Years$",
    "pay_state_group", "^State Payment Totals and Averages Grouped by Nature of Payment for all Years$",
    "pay_nat_group",   "^National Level Payment Total and Averages by Provider Specialty for all Years$",
    "pay_nat_total",   "^National Level Payment Total and Averages for all Years$",
    default = NA_character_,
    nThread = 4L
  )
}

#' @noRd
alias_open_temp <- function(x) {
  nswitch(
    x,
    "general",                 "^General Payment Data$",
    "ownership",               "^Ownership Payment Data$",
    "research",                "^Research Payment Data$",
    "recipient_nature",        "^Payments Grouped by Covered Recipient and Nature of Payments$",
    "recipient_entity",        "^Payments Grouped by Covered Recipient and Reporting Entities$",
    "entity_nature",           "^Payments Grouped by Reporting Entities and Nature of Payments$",
    "entity_recipient_nature", "^Payments Grouped by Reporting Entities, Covered Recipient, and Nature of Payments$",
    "state_nature",            "^State Payment Totals Grouped by Nature of Payment for all Years$",
    default = NA_character_,
    nThread = 4L
  )
}
