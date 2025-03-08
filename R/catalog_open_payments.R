#' Join Vector of Download URLs to Main Dataset
#' @param x data.frame
#' @returns data.frame
#' @export
#' @autoglobal
#' @keywords internal
open_add_dlurl <- function(x) {
  add_vars(
    x,
    downloadurl = delist(
      get_elem(
        get_elem(
          x$distribution,
          "data",
          DF.as.list = TRUE),
        "downloadURL"
        )
      )
    )
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

#' Convert Open Payments UUID to URL
#' @param x `<chr>` UUID
#' @returns `<chr>` URL
#' @export
#' @autoglobal
#' @keywords internal
open_uuid_url <- function(x) {
  paste0(
    "https://openpaymentsdata.cms.gov/",
    "api/1/datastore/query/",
    x,
    "/0")
}

#' Load Open Payments Catalog
#' @returns `<list>` of Open Payments API catalog information
#' @examples
#' open_catalog()
#' @autoglobal
#' @export
open_catalog <- function() {
  x <- fload(
    paste0(
      "https://openpaymentsdata.cms.gov",
      "/api/1/metastore/schemas/dataset/",
      "items?show-reference-ids"
    )
  ) |>
    as_tbl() |>
    mtt(
      modified    = as_date(modified),
      description = replace_fixed(description, c("\n", "\r. \r.", '"'), c(". ", "", "")),
      theme       = get_data_elem(theme),
      year        = get_data_elem(keyword),
      year        = replace_fixed(year, c("all years"), c("All")),
      year        = cheapr_if_else(title == "Provider profile ID mapping table", "All", year),
      title       = toTitleCase(title)
    )

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
  s <- subset_detect(open_catalog()$summary,
                     title,
                     "^(National|State|Payments|Summary)",
                     TRUE)

  list(modified = fmax(s$modified),
       summary  = slt(s, -modified))
}

#' Load Open Payments Grouped
#' @returns `<list>` of Open Payments API grouped information
#' @examples
#' open_grouped()
#' @autoglobal
#' @export
open_grouped <- function() {
  s <- subset_detect(open_catalog()$summary,
                     title,
                     "^(National|State|Payments|Summary)")

  g <- slt(open_catalog()$grouped, -modified) |>
    rsplit(~ title)

  list(
    modified                = fmax(c(s$modified, open_catalog()$grouped$modified)),
    summary                 = slt(s, -modified, -description),
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
  list(
    modified    = fmax(open_catalog()$general$modified),
    title       = "General Payment Data",
    description = "All general (non-research, non-ownership related) payments from the program year",
    endpoints   = slt(open_catalog()$general, -modified)
  )
}

#' Load Open Payments Research Data
#' @returns `<list>` of Open Payments API research data
#' @examples
#' open_research()
#' @autoglobal
#' @export
open_research <- function() {
  list(
    modified    = fmax(open_catalog()$research$modified),
    title       = "Research Payment Data",
    description = "All research-related payments from the program year",
    endpoints   = slt(open_catalog()$research, -modified)
  )
}

#' Load Open Payments Ownership Data
#' @returns `<list>` of Open Payments API ownership data
#' @examples
#' open_ownership()
#' @autoglobal
#' @export
open_ownership <- function() {
  list(
    modified    = fmax(open_catalog()$ownership$modified),
    title       = "Ownership Payment Data",
    description = "All ownership and investment payments from the program year",
    endpoints   = slt(open_catalog()$ownership, -modified)
  )
}

#' Load Open Payments API `Dataset`
#' @param period `<chr>` dataset year
#' @param group `<chr>` dataset group keyword
#' @param dataset `<chr>` dataset title
#' @returns `<Dataset>` object
#' @examples
#' open_dataset(period  = "All",
#'              group   = "Summary",
#'              dataset = "Covered Recipient Profile Supplement")
#' @autoglobal
#' @export
open_dataset <- function(period  = NULL,
                         group   = NULL,
                         dataset = NULL) {
  if (!exists(".api__openpay"))
    .api__openpay <<- load_openpayments()

  if (not_null(period))
    x <- sbt(.api__openpay, sf_detect(year, period))
  if (not_null(group))
    x <- sbt(.api__openpay, sf_detect(theme, group))
  if (not_null(dataset))
    x <- sbt(.api__openpay, sf_detect(title, dataset))

  c(x)

  # Dataset(
  #   contact     = Contact("Open Payments", "mailto:openpayments@cms.hhs.gov"),
  #   publisher   = Publisher(name = "openpaymentsdata.cms.gov"),
  #   periodicity = "Annually [R/P1Y]",
  #   identifier  = Identifier(x$identifier),
  #   modified    = x$modified,
  #   title       = x$title,
  #   dictionary  = x$describedBy,
  #   description = x$description,
  #   landingpage = x$landingPage,
  #   temporal    = x$temporal,
  #   theme       = x$theme
  # )
}
