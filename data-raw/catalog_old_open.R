#' Load `<TemporalOpen>` API Endpoint
#' @param alias `<chr>` dataset title
#' @returns `<TemporalOpen>` object
#' @examples
#' open_temporal("general")
#' open_temporal("ownership")
#' open_temporal("research")
#' open_temporal("recipient_nature")
#' open_temporal("recipient_entity")
#' open_temporal("entity_nature")
#' open_temporal("entity_recipient_nature")
#' open_temporal("state_nature")
#' @autoglobal
#' @export
open_temporal <- function(alias) {

  x <- catalog_open()$temporal |>
    subset_detect(
      title,
      alias_open_temporal(alias))

  q <- open_nrows_fields(x$identifier[1])

  TemporalOpen(
    title       = x$title[1],
    description = x$description[1],
    contact     = x$contact[1],
    rows        = q$rows,
    fields      = q$fields,
    pages       = q$pages,
    years       = x$year,
    endpoints   = slt(x, year, modified, identifier, download)
  )
}

#' Load `<CurrentOpen>` API Endpoint
#' @param alias `<chr>` endpoint alias
#' @returns `<CurrentOpen>` object
#' @examples
#' open_current("prof_cov")
#' open_current("prof_phys")
#' open_current("prof_info")
#' open_current("prof_map")
#' open_current("prof_entity")
#' open_current("prof_teach")
#' open_current("dashboard")
#' open_current("pay_state_total")
#' open_current("pay_state_group")
#' open_current("pay_nat_group")
#' open_current("pay_nat_total")
#' @autoglobal
#' @export
open_current <- function(alias) {

  x <- catalog_open()$current |>
    subset_detect(
      title,
      alias_open_current(alias)) |>
    c()

  q <- open_nrows_fields(x$identifier)

  CurrentOpen(
    title       = x$title,
    description = x$description,
    contact     = x$contact,
    modified    = x$modified,
    uuid        = x$identifier,
    download    = x$download,
    rows        = q$rows,
    fields      = q$fields,
    pages       = q$pages
  )
}


#' Join Vector of Download URLs to Main Dataset
#' @param x `<data.frame>`
#' @returns `<data.frame>`
#' @autoglobal
#' @keywords internal
#' @export
open_add_dlurl <- function(x) {
  add_vars(x, download = delist(get_elem(get_elem(x$distribution, "data", DF.as.list = TRUE), "downloadURL")))
}

#' Clean Open Payments Temporal Data
#' @param x data.frame
#' @returns data.frame
#' @export
#' @autoglobal
#' @keywords internal
open_clean_temp <- function(x) {
  mtt(x, year = as_int(year)) |>
    slt(-title, -description) |>
    roworder(-year)
}

#' Clean Open Payments Grouped Data
#' @param x data.frame
#' @returns data.frame
#' @export
#' @autoglobal
#' @keywords internal
open_clean_grouped <- function(x) {
  sbt(x, year != "All", -description) |>
    mtt(year   = as_int(year),
        title  = stri_replace_all_regex(title, "^[0-9]{4} ", "")) |>
    roworder(title, -year)
}

#' Load Open Payments Catalog
#' @returns `<list>` of Open Payments API catalog information
#' @autoglobal
#' @export
#' @keywords internal
catalog_open_ <- function() {
  x <- fload(
    paste0(
      "https://openpaymentsdata.cms.gov",
      "/api/1/metastore/schemas/dataset/",
      "items?show-reference-ids"
    )) |>
    mtt(
      modified = as_date(modified),
      description = replace_fixed(description,
                                  c("\n", "\r. \r.",'"', paste0(
                                    "<p><strong>NOTE: ",
                                    "</strong>This is a very large file and, ",
                                    "depending on your network characteristics and software, ",
                                    "may take a long time to download or fail to download. ",
                                    "Additionally, the number of rows in the file may be larger ",
                                    "than the maximum rows your version of <a href=\"https://support.",
                                    "microsoft.com/en-us/office/excel-specifications-and-limits-",
                                    "1672b34d-7043-467e-8e27-269d656771c3\">Microsoft Excel</a> supports. ",
                                    "If you can't download the file, we recommend engaging your IT support staff. ",
                                    "If you are able to download the file but are unable to open it in MS Excel or ",
                                    "get a message that the data has been truncated, we recommend trying alternative ",
                                    "programs such as MS Access, Universal Viewer, Editpad or any other software your ",
                                    "organization has available for large datasets.</p>")),
                                  c(". ", "", "", "")),
      theme = get_data_elem(theme),
      year = get_data_elem(keyword),
      year = replace_fixed(year, c("all years"), c("All")),
      year = cheapr_if_else(title == "Provider profile ID mapping table", "All", year),
      title = toTitleCase(title)) |>
    as_tbl()

  x <- open_add_dlurl(x) |>
    slt(year,
        theme,
        title,
        description,
        modified,
        identifier,
        downloadurl) |>
    rsplit( ~ theme)

  list(
    summary   = roworder(sbt(x$Summary, year == "All", -year), title),
    grouped   = open_clean_grouped(x$Summary),
    general   = open_clean_temp(x$`General Payments`),
    research  = open_clean_temp(x$`Research Payments`),
    ownership = open_clean_temp(x$`Ownership Payments`)
  )
}

#' Load Open Payments Profile Sets
#' @returns `<list>` of Open Payments API profile information
#' @autoglobal
#' @export
#' @keywords internal
open_profiles <- function() {
  s <- subset_detect(catalog_open_()$summary,
                     title,
                     "^(National|State|Payments|Summary)",
                     TRUE)

  list(modified = fmax(s$modified),
       summary  = slt(s, -modified))
}

#' Load Open Payments Grouped Summaries
#' @returns `<list>` of Open Payments API grouped information
#' @autoglobal
#' @export
#' @keywords internal
open_grouped_summary <- function() {
  s <- subset_detect(catalog_open_()$summary,
                     title,
                     "^(National|State|Payments|Summary)")

  list(modified = fmax(s$modified),
       summary = slt(s, -modified, -description))
}

#' Load Open Payments Grouped by Year
#' @returns `<list>` of Open Payments API grouped information
#' @autoglobal
#' @export
#' @keywords internal
open_grouped_yearly <- function() {
  g <- slt(catalog_open_()$grouped, -modified) |>
    rsplit( ~ title)

  list(
    modified                = fmax(catalog_open_()$grouped$modified),
    recipient_nature        = g$`Payments Grouped by Covered Recipient and Nature of Payments`,
    recipient_entity        = g$`Payments Grouped by Covered Recipient and Reporting Entities`,
    entity_nature           = g$`Payments Grouped by Reporting Entities and Nature of Payments`,
    entity_recipient_nature = g$`Payments Grouped by Reporting Entities, Covered Recipient, and Nature of Payments`,
    recipient_nature_state  = g$`State Payment Totals Grouped by Nature of Payment for all Years`
  )
}

#' Load Open Payments General Data
#' @returns `<list>` of Open Payments API general data
#' @autoglobal
#' @export
#' @keywords internal
open_general <- function() {

  x <- catalog_open_()$general

  q <- open_nrows_fields(delist(ss(x, 1, 3)))

  list(
    title       = "General Payment Data",
    description = "All general (non-research, non-ownership related) payments from the program year",
    modified    = fmax(x$modified),
    years       = sort(x$year),
    nrows       = q$rows,
    nfields     = fnobs(q$fields),
    endpoints   = slt(x, -modified),
    fields      = q$fields
  )
}

#' Load Open Payments Research Data
#' @returns `<list>` of Open Payments API research data
#' @autoglobal
#' @export
open_research <- function() {

  x <- catalog_open_()$research

  q <- open_nrows_fields(delist(ss(x, 1, 3)))

  list(
    title       = "Research Payment Data",
    description = "All research-related payments from the program year",
    modified    = fmax(x$modified),
    years       = sort(x$year),
    nrows       = q$rows,
    nfields     = fnobs(q$fields),
    endpoints   = slt(x, -modified),
    fields      = q$fields
  )
}

#' Load Open Payments Ownership Data
#' @returns `<list>` of Open Payments API ownership data
#' @autoglobal
#' @export
open_ownership <- function() {

  x <- catalog_open_()$research

  q <- open_nrows_fields(delist(ss(x, 1, 3)))

  list(
    title       = "Ownership Payment Data",
    description = "All ownership and investment payments from the program year",
    modified    = fmax(x$modified),
    years       = sort(x$year),
    nrows       = q$rows,
    nfields     = fnobs(q$fields),
    endpoints   = slt(x, -modified),
    fields      = q$fields
  )
}
