#' Load Open Payments Catalog
#' @returns `<list>` of Open Payments API catalog information
#' @examples
#' catalog_open()
#' @autoglobal
#' @export
catalog_open <- function() {
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
#' @examples
#' open_profiles()
#' @autoglobal
#' @export
open_profiles <- function() {
  s <- subset_detect(catalog_open()$summary,
                     title,
                     "^(National|State|Payments|Summary)",
                     TRUE)

  list(modified = fmax(s$modified),
       summary  = slt(s, -modified))
}

#' Load Open Payments Grouped Summaries
#' @returns `<list>` of Open Payments API grouped information
#' @examples
#' open_grouped_summary()
#' @autoglobal
#' @export
open_grouped_summary <- function() {
  s <- subset_detect(catalog_open()$summary,
                     title,
                     "^(National|State|Payments|Summary)")

  list(modified = fmax(s$modified),
       summary = slt(s, -modified, -description))
}

#' Load Open Payments Grouped by Year
#' @returns `<list>` of Open Payments API grouped information
#' @examples
#' open_grouped_yearly()
#' @autoglobal
#' @export
open_grouped_yearly <- function() {
  g <- slt(catalog_open()$grouped, -modified) |>
    rsplit( ~ title)

  list(
    modified                = fmax(catalog_open()$grouped$modified),
    recipient_nature        = g$`Payments Grouped by Covered Recipient and Nature of Payments`,
    recipient_entity        = g$`Payments Grouped by Covered Recipient and Reporting Entities`,
    entity_nature           = g$`Payments Grouped by Reporting Entities and Nature of Payments`,
    entity_recipient_nature = g$`Payments Grouped by Reporting Entities, Covered Recipient, and Nature of Payments`,
    recipient_nature_state  = g$`State Payment Totals Grouped by Nature of Payment for all Years`
  )
}

#' Load Open Payments General Data
#' @returns `<list>` of Open Payments API general data
#' @examples
#' open_general()
#' @autoglobal
#' @export
open_general <- function() {

  x <- catalog_open()$general

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
#' @examples
#' open_research()
#' @autoglobal
#' @export
open_research <- function() {

  x <- catalog_open()$research

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
#' @examples
#' open_ownership()
#' @autoglobal
#' @export
open_ownership <- function() {

  x <- catalog_open()$research

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
