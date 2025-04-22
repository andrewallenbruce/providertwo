
care_temp_group <- function(x, call = caller_env()) {

  x <- switch(
    x,
    inpatient       = "Medicare Inpatient Hospitals",
    outpatient      = "Medicare Outpatient Hospitals",
    # "Medicare Part D (Prescribers|Opioid|Spending)",
    prescribers     = "Medicare Part D Prescribers",
    suppliers       = "Medicare Durable Medical Equipment, Devices & Supplies",
    staffing        = "^Payroll Based Journal",
    utilization     = "Medicare Physician & Other Practitioners",
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call))

  if (!exists("catalog")) .catalog <- catalogs()

  x <- select_alias(.catalog$care$temp, x)

  if (all(stri_detect_regex(x$title, "^Payroll"))) {

    x <- mtt(x, group = clean_names(stri_extract_first_regex(title, "(?<=Payroll Based Journal\\s).*$")), title = "Payroll Based Journal")

  } else {

    x <- mtt(x, group = clean_names(stri_extract_first_regex(title, "(?<=-\\s).*$")), title = stri_extract_first_regex(title, "^.*(?=\\s-)"))

  }

  x2 <- map2(x$data,  x$group, \(a, b) slt(mtt(a, group = clean_names(b)), year, group, identifier, resources)) |> rowbind() |>
    join(slt(x, -data, -title, -periodicity, -contact), on = "group", verbose = 0)

  years  <- funique(x2$year)
  groups <- funique(x2$group)

  q <- dims_care_temp_group(x = sbt(x2, year == fmax(year)) |> _[["identifier"]], g = groups)

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

#' Group of Medicare Temporal Endpoints
#'
#' @param alias `<chr>` title alias
#'
#' @returns An S7 `<careTempGroup>` object.
#'
#' @examplesIf interactive()
#' careTempGroup("inpatient")
#' careTempGroup("outpatient")
#' careTempGroup("utilization")
#' careTempGroup("suppliers")
#' careTempGroup("prescribers")
#' careTempGroup("staffing")
#' @autoglobal
#' @rdname careTempGroup
#' @export
careTempGroup <- new_class(
  parent     = Care,
  name       = "careTempGroup",
  package    = NULL,
  properties = list(
    title       = class_character,
    periodicity = class_character,
    years       = class_integer,
    groups      = class_list
  ),
  constructor = function(alias) {

    x <- care_temp_group(alias)

    template <- glue(
      "
      {group} = list(
        description = x${group}$description,
        dictionary  = x${group}$dictionary,
        site        = x${group}$site,
        rows        = x${group}$rows,
        pages       = x${group}$pages,
        fields      = x${group}$fields,
        endpoints   = x${group}$endpoints
        )
      ",
      group = x$groups) |>
      glue_collapse(sep = ",\n")

    new_object(
      Care(),
      title        = x$title,
      periodicity  = x$periodicity,
      years        = sort(x$years),
      groups       = glue("list({template})") |> parse_expr() |> eval_bare()
    )
  }
)
