#' Join Vector of Download URLs to Main Dataset
#' @param x data.frame
#' @returns data.frame
#' @export
#' @autoglobal
#' @keywords internal
add_downloadurl <- function(x) {
  add_vars(x, downloadurl = delist(get_elem(
    get_elem(x$distribution, "data", DF.as.list = TRUE),
    "downloadURL"
  )))
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
#' catalog_open_payments()
#' @autoglobal
#' @export
catalog_open_payments <- \() {

  x <- fload(
    paste0(
      "https://openpaymentsdata.cms.gov",
      "/api/1/metastore/schemas/dataset/",
      "items?show-reference-ids")) |>
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

  x <- add_downloadurl(x) |>
    slt(year,
        theme,
        title,
        description,
        modified,
        identifier,
        downloadurl) |>
    rsplit(~ theme)

  list(
    summary   = roworder(sbt(x$Summary, year == "All", -year), title),
    grouped   = open_clean_grouped(x$Summary),
    general   = open_clean_temp(x$`General Payments`),
    research  = open_clean_temp(x$`Research Payments`),
    ownership = open_clean_temp(x$`Ownership Payments`)
  )
}
