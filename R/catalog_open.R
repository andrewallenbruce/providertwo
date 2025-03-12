#' Get Field Names and Number of Rows from Open Payments Endpoint
#' @param uuid `<chr>` endpoint UUID
#' @returns `<list>` number of rows and field names
#' @autoglobal
#' @keywords internal
#' @export
open_nrows_fields <- function(uuid) {

  x <- uuid |>
    open_uuid_url() |>
    request() |>
    req_url_query(
      schema  = "false",
      keys    = "false",
      results = "false",
      count   = "true",
      offset  = 0,
      limit   = 1
    ) |>
    req_perform() |>
    resp_simple_json()

  list(rows   = x$count,
       fields = x$query$properties,
       pages  = offset_length(x$count, 500L))

}

#' Join Vector of Download URLs to Main Dataset
#' @param x `<data.frame>`
#' @returns `<data.frame>`
#' @autoglobal
#' @keywords internal
#' @export
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

#' Convert Open Payments UUID to URL
#' @param uuid `<chr>` endpoint UUID
#' @returns `<chr>` endpoint URL
#' @autoglobal
#' @keywords internal
#' @export
open_uuid_url <- function(uuid) {
  paste0(
    "https://openpaymentsdata.cms.gov/",
    "api/1/datastore/query/",
    uuid,
    "/0")
}

#' Open Payments Catalog
#' @returns `<list>` of Open Payments API catalog information
#' @autoglobal
#' @keywords internal
#' @export
catalog_open <- function() {

  x <- fload(paste0("https://openpaymentsdata.cms.gov",
                    "/api/1/metastore/schemas/dataset/",
                    "items?show-reference-ids"))

  x <- x |> mtt(
      modified    = as_date(modified),
      theme       = get_data_elem(theme),
      year        = get_data_elem(keyword),
      year        = replace_fixed(year, c("all years"), c("All")),
      year        = cheapr_if_else(title == "Provider profile ID mapping table", "All", year),
      title       = toTitleCase(title),
      contact     = as.character(glue('{delist(get_elem(x$contactPoint, "fn"))} ({delist(get_elem(x$contactPoint, "^has", regex = TRUE))})')),
      description = sf_remove(description, "[\"']"),
      description = gsub("\r\n", " ", description, perl = TRUE),
      description = sf_remove(description, "<p><strong>NOTE: </strong>This is a very large file and, depending on your network characteristics and software, may take a long time to download or fail to download. Additionally, the number of rows in the file may be larger than the maximum rows your version of <a href=https://support.microsoft.com/en-us/office/excel-specifications-and-limits-1672b34d-7043-467e-8e27-269d656771c3>Microsoft Excel</a> supports. If you cant download the file, we recommend engaging your IT support staff. If you are able to download the file but are unable to open it in MS Excel or get a message that the data has been truncated, we recommend trying alternative programs such as MS Access, Universal Viewer, Editpad or any other software your organization has available for large datasets.</p>$"),
      description = gsub("  ", " ", description, perl = TRUE),
      description = stri_trim(description)) |>
    open_add_dlurl() |>
    slt(year,
        theme,
        title,
        description,
        modified,
        identifier,
        contact,
        download = downloadurl) |>
    as_tbl()

  list(current  = sbt(x, year == "All", -theme, -year) |> roworder(title),
       temporal = sbt(x, year != "All", -theme) |> mtt(year = as_int(year), title = stri_replace_all_regex(title, "^[0-9]{4} ", "")) |> roworder(title, -year))
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
