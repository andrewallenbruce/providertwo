#' CMS Main Catalog
#' @returns `<list>` of CMS Main API catalog information
#' @examplesIf rlang::is_interactive()
#' catalog_main()
#' @autoglobal
#' @keywords internal
#' @export
catalog_main <- function() {

  x <- fload("https://data.cms.gov/data.json", query = "/dataset")

  x <- mtt(x,
      modified    = as_date(modified),
      periodicity = recode_iso8601(accrualPeriodicity),
      contact     = fmt_contact(x$contactPoint),
      references  = delist(references),
      temporal    = fmt_temp(temporal),
      title       = gsub("  ", " ", title, perl = TRUE),
      description = gsub("[\"']", "", description, perl = TRUE),
      description = gsub("Note: This full dataset contains more records than most spreadsheet programs can handle, which will result in an incomplete load of data. Use of a database or statistical software is required.$", "", description, perl = TRUE),
      description = gsub("^ATTENTION USERS\\n\\n\\nSome Providers Opt-Out Status may end early due to COVID 19 waivers. Please contact your respective MAC for further information.\\n\\n.+\\n\\nFor more information on the opt-out process, see Manage Your Enrollment.+or view the FAQ section below.\\n\\n.+\\n\\n", "", description, perl = TRUE),
      description = gsub("\\n\\n.+\\n\\nOn November 17, 2023, CMS published in the Federal Register a final rule titled, .+Medicare and Medicaid Programs; Disclosures of Ownership and Additional Disclosable Parties Information for Skilled Nursing Facilities and Nursing Facilities; Medicare Providers.+ and Suppliers.+ Disclosure of Private Equity Companies and Real Estate Investment Trusts.+ .+88 FR 80141.+. This final rule implements parts of section 1124.+c.+ \\n\\n.+\\n\\n.+", "", description, perl = TRUE),
      description = stri_trim(description)) |>
    slt(
      title,
      description,
      modified,
      periodicity,
      temporal,
      contact,
      identifier,
      dictionary  = describedBy,
      site        = landingPage,
      references,
      distribution) |>
    as_tbl()

  d <- rowbind(x$distribution, fill = TRUE) |>
    fcompute(
      year       = as_int(stri_extract_all_regex(title, "[0-9]{4}")),
      title      = stri_replace_all_regex(title, " : [0-9]{4}-[0-9]{2}-[0-9]{2}([0-9A-Za-z]{1,3})?$", ""),
      format     = cheapr_if_else(not_na(description), description, format),
      modified   = as_date(modified),
      temporal   = fmt_temp(temporal),
      identifier = accessURL,
      download   = lag_(downloadURL, n = -1L),
      filetype   = lag_(mediaType, n = -1L),
      resources  = resourcesAPI) |>
    colorder(title) |>
    as_tbl()

  d <- sset(d, row_na_counts(d) < 4) |> funique(cols = c("title", "year", "format"))

  list_tidy(
    current = join_on_title(
      slt(x, -distribution),
      sbt(d, format == "latest", -format, -identifier, -modified, -temporal)) |>
      roworder(title),
    temporal = join_on_title(
      sbt(d, format != "latest", -format) |> roworder(title, -year) |> f_nest_by(.cols = "title") |> f_ungroup(),
      slt(current, title, description, periodicity, contact, dictionary, site))
    )
}

#' Main Endpoint Group
#' @param alias `<chr>` title alias
#' @returns `<list>` of a group of main endpoints
#' @examplesIf rlang::is_interactive()
#' main_group("hospitals")
#' main_group("rhc")
#' main_group("fqhc")
#' main_group("pending")
#' @autoglobal
#' @noRd
main_group <- function(alias) {

  if (!exists("catalog")) catalog <- catalogs()

  select_alias(catalog$main$current, alias_main_group(alias)) |>
    slt(-filetype)
}

#' Temporal Main Endpoint Group
#' @param alias `<chr>` title alias
#' @returns `<list>` of a group of temporal main endpoints
#' @examplesIf rlang::is_interactive()
#' main_temp_group("utilization")
#' main_temp_group("prescribers")
#' main_temp_group("suppliers")
#' main_temp_group("outpatient")
#' main_temp_group("inpatient")
#' @autoglobal
#' @noRd
main_temp_group <- function(alias) {

  if (!exists("catalog")) catalog <- catalogs()

  x <- select_alias(catalog$main$temporal, alias_main_temp_group(alias)) |>
    mtt(
      group = clean_names(stri_extract_first_regex(title, "(?<=-\\s).*$")),
      title = stri_extract_first_regex(title, "^.*(?=\\s-)")
    ) |>
    colorder(title, group)

  x2 <- map2(x$data,  x$group, \(x, y)
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
