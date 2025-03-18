#' Convert Provider UUID to URL
#' @param uuid `<chr>` endpoint UUID
#' @returns `<chr>` endpoint URL
#' @export
#' @autoglobal
#' @keywords internal
pro_url <- function(uuid) {
  paste0("https://data.cms.gov/",
         "provider-data/api/1/",
         "datastore/query/",
         uuid,
         "/0")
}

#' Convert Provider UUID to Data Dictionary Hyperlink
#' @param uuid `<chr>` endpoint UUID
#' @returns `<chr>` dictionary URL
#' @export
#' @autoglobal
#' @keywords internal
pro_dict <- function(uuid) {
  paste0("https://data.cms.gov/",
         "provider-data/dataset/",
         uuid,
         "#data-dictionary")
}

#' Get Field Names and Number of Rows from Provider Endpoint
#' @param uuid `<chr>` endpoint UUID
#' @returns `<list>` number of rows and field names
#' @autoglobal
#' @keywords internal
#' @export
pro_dims <- function(uuid) {

  x <- uuid |>
    pro_url() |>
    request() |>
    req_url_query(
      schema  = "false",
      keys    = "false",
      results = "false",
      count   = "true",
      offset  = 0,
      limit   = 1
    ) |>
    perform_simple()

  list(rows   = x$count,
       fields = x$query$properties,
       pages  = offset_length(x$count, 2000L))

}

#' Load `<CurrentProviderGroup>` API Endpoint
#' @param alias `<chr>` endpoint alias
#' @returns `<CurrentProviderGroup>` object
#' @examples
#' provider_current_group("mips")
#' @autoglobal
#' @export
provider_current_group <- function(alias) {
  subset_detect(catalog_provider(),
                title,
                alias_provider_current_group(alias))
}

#' CMS Provider Catalog
#' @returns `<list>` of Provider API catalog information
#' @examples
#' catalog_provider()
#' @autoglobal
#' @export
catalog_provider <- function() {

  x <- fload(
    paste0(
      "https://data.cms.gov/",
      "provider-data/api/1/",
      "metastore/schemas/dataset/items"
    )
  )

  mtt(x,
      issued      = as_date(issued),
      modified    = as_date(modified),
      released    = as_date(released),
      group       = flatten_column(theme),
      description = stri_trim(gsub("\n", "", description, perl = TRUE)),
      download    = delist_elem(x$distribution, "downloadURL"),
      contact     = reduce_contact(x$contactPoint)) |>
    slt(title, group, description, issued, modified, released, identifier, contact, download, site = landingPage) |>
    roworder(group, title) |>
    as_tbl()
}
