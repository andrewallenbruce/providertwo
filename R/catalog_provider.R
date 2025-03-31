
#' @noRd
pro_url <- \(x) paste0("https://data.cms.gov/provider-data/api/1/datastore/query/", x, "/0")

#' @noRd
pro_dict <- \(x) paste0("https://data.cms.gov/provider-data/dataset/", x, "#data-dictionary")

#' CMS Provider Catalog
#'
#' @returns `<list>` of Provider API catalog information
#'
#' @examplesIf rlang::is_interactive()
#' catalog_provider()
#'
#' @autoglobal
#' @keywords internal
#' @export
catalog_provider <- function() {

  x <- fload("https://data.cms.gov/provider-data/api/1/metastore/schemas/dataset/items")

  mtt(x,
      issued      = as_date(issued),
      modified    = as_date(modified),
      released    = as_date(released),
      group       = flatten_column(theme),
      description = stri_trim(gsub("\n", "", description, perl = TRUE)),
      download    = delist_elem(x$distribution, "downloadURL"),
      contact     = fmt_contact(x$contactPoint)) |>
    slt(title, group, description, issued, modified, released, identifier, contact, download, site = landingPage) |>
    roworder(group, title) |>
    as_tbl()
}
