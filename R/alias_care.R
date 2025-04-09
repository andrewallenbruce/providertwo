# care_main("enrollees")
# care_main("opt_out")
# care_main("order_refer")
# care_main("reassignments")
# #care_main("hospitals")
# care_main("laboratories")
# care_main("crosswalk")
# care_main("rbcs")
# care_main("facilities")
# care_main("home_health")
# care_main("hospice")
# care_main("dialysis")
# care_main("skilled_nursing")
#' @autoglobal
#' @noRd
care_main <- function(x, call = caller_env()) {

  x <- switch(
    x,
    crosswalk       = "Medicare Provider and Supplier Taxonomy Crosswalk",
    dialysis        = "Medicare Dialysis Facilities",
    enrollees       = "Public Provider Enrollment",
    facilities      = "Provider of Services File - Hospital & Non-Hospital Facilities",
    home_health     = "Home Health Agency Enrollments",
    hospice         = "Hospice Enrollments",
    # hospitals       = "Hospital Enrollments",
    laboratories    = "Provider of Services File - Clinical Laboratories",
    opt_out         = "Opt Out Affidavits",
    order_refer     = "Order and Referring",
    rbcs            = "Restructured BETOS Classification System",
    reassignments   = "Revalidation Reassignment List",
    skilled_nursing = "Skilled Nursing Facility Enrollments",
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call))

  if (!exists("catalog")) .catalog <- catalogs()

  select_alias(.catalog$care$main, x) |> c()

}

# care_group("hospitals")
# care_group("rhc")
# care_group("fqhc")
# care_group("pending")
#' @autoglobal
#' @noRd
care_group <- function(x, call = caller_env()) {

  x <- switch(
    x,
    hospitals       = "^Hospital [ACE]",
    rhc             = "Rural Health Clinic",
    fqhc            = "Federally Qualified Health Center",
    pending         = "Pending Initial Logging and Tracking",
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call))

  if (!exists("catalog")) .catalog <- catalogs()

  select_alias(.catalog$care$main, x) |> slt(-filetype)
}

# care_temp("quality_payment")
#' @autoglobal
#' @noRd
care_temp <- function(x, call = caller_env()) {

  x <- switch(
    x,
    quality_payment = "Quality Payment Program Experience",
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call))

  if (!exists("catalog")) .catalog <- catalogs()

  select_alias(.catalog$care$temp, x)

}

# care_temp_group("inpatient")
# care_temp_group("outpatient")
# care_temp_group("prescribers")
# care_temp_group("suppliers")
# care_temp_group("utilization")
#' @autoglobal
#' @noRd
care_temp_group <- function(x, call = caller_env()) {

  x <- switch(
    x,
    inpatient       = "Medicare Inpatient Hospitals",
    outpatient      = "Medicare Outpatient Hospitals",
    prescribers     = "Medicare Part D Prescribers",
    suppliers       = "Medicare Durable Medical Equipment, Devices & Supplies",
    utilization     = "Medicare Physician & Other Practitioners",
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call))

  if (!exists("catalog")) .catalog <- catalogs()

  x <- select_alias(.catalog$care$temp, x) |>
    mtt(group = stri_extract_first_regex(title, "(?<=-\\s).*$") |> clean_names(),
        title = stri_extract_first_regex(title, "^.*(?=\\s-)"))

  x2 <- map2(x$data,  x$group, function(x, y) {
    mtt(x, group = clean_names(y)) |>
      slt(year, group, identifier, resources)
    }) |>
    rowbind() |>
    join(
      slt(x, -data, -title, -periodicity, -contact),
      on = "group",
      verbose = 0)

  years  <- funique(x2$year)
  groups <- funique(x2$group)

  q <- dims_care_temp_group(
    x = sbt(x2, year == fmax(year)) |> _[["identifier"]],
    g = groups)

  x2 <- rsplit(x2, ~ group)

  template <- glue(
    "
  {group} = list(
  description = x2${group}$description[1],
  dictionary  = x2${group}$dictionary[1],
  site        = x2${group}$site[1],
  rows        = q$rows${group},
  pages       = q$pages${group},
  fields      = q$fields${group},
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
