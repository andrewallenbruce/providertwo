#' @noRd
open_url <- function(x) paste0("https://openpaymentsdata.cms.gov/api/1/datastore/query/", x, "/0")

#' CMS Open Payments Catalog
#'
#' @returns `<list>` of Open Payments API catalog information
#'
#' @examplesIf rlang::is_interactive()
#' catalog_open()
#'
#' @autoglobal
#' @keywords internal
#' @export
catalog_open <- function() {

  x <- fload("https://openpaymentsdata.cms.gov/api/1/metastore/schemas/dataset/items?show-reference-ids")

  x <- mtt(x,
      modified    = as_date(modified),
      year        = get_data_elem(keyword),
      year        = replace_fixed(year, c("all years"), c("All")),
      year        = cheapr_if_else(title == "Provider profile ID mapping table", "All", year),
      title       = toTitleCase(title),
      contact     = fmt_contact(x$contactPoint),
      description = gsub("[\"']", "", description, perl = TRUE),
      description = gsub("\r\n", " ", description, perl = TRUE),
      description = gsub("<p><strong>NOTE: </strong>This is a very large file and, depending on your network characteristics and software, may take a long time to download or fail to download. Additionally, the number of rows in the file may be larger than the maximum rows your version of <a href=https://support.microsoft.com/en-us/office/excel-specifications-and-limits-1672b34d-7043-467e-8e27-269d656771c3>Microsoft Excel</a> supports. If you cant download the file, we recommend engaging your IT support staff. If you are able to download the file but are unable to open it in MS Excel or get a message that the data has been truncated, we recommend trying alternative programs such as MS Access, Universal Viewer, Editpad or any other software your organization has available for large datasets.</p>$", "", description, perl = TRUE),
      description = gsub("  ", " ", description, perl = TRUE),
      description = stri_trim(description),
      download    = delist(get_elem(get_elem(x$distribution, "data", DF.as.list = TRUE), "downloadURL"))) |>
    slt(year, title, description, modified, identifier, contact, download) |>
    as_tbl()

  list(current  = roworder(sbt(x, year == "All", -year), title),
       temporal = roworder(sbt(x, year != "All") |> mtt(year = as_int(year), title = stri_replace_all_regex(title, "^[0-9]{4} ", "")), title, -year)
  )
}
