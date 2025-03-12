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
  ) |>
    as_tbl() |>
    mtt(
      issued      = as_date(issued),
      modified    = as_date(modified),
      released    = as_date(released),
      theme       = flatten_column(theme),
      description = trimws(sf_remove(description, "\n"))
    )

  add_vars(x,
    downloadurl = delist(get_elem(
      x$distribution, "downloadURL", DF.as.list = TRUE)),
    contact = as.character(
      glue('{delist(get_elem(x$contactPoint, "fn"))} ',
           '({delist(get_elem(x$contactPoint, "^has", regex = TRUE))})'))) |>
    slt(
      title,
      theme,
      description,
      issued,
      modified,
      released,
      identifier,
      contact,
      download = downloadurl,
      site = landingPage
    ) |>
    roworder(theme, title) |>
    rsplit(~ theme) |>
    set_names(
      c(
        "dialysis_facilities",
        "doctors_and_clinicians",
        "home_health_services",
        "hospice_care",
        "hospitals",
        "inpatient_rehabilitation_facilities",
        "long_term_care_hospitals",
        "nursing_homes",
        "physician_office_visit_costs",
        "supplier_directory"
      )
    )
}

#' Convert Provider UUID to URL
#' @param uuid `<chr>` endpoint UUID
#' @returns `<chr>` endpoint URL
#' @export
#' @autoglobal
#' @keywords internal
prov_uuid_url <- function(uuid) {
  paste0(
    "https://data.cms.gov/",
    "provider-data/api/1/",
    "datastore/query/",
    uuid, "/0")
}

#' Convert Provider UUID to Data Dictionary Hyperlink
#' @param uuid `<chr>` endpoint UUID
#' @returns `<chr>` dictionary URL
#' @export
#' @autoglobal
#' @keywords internal
prov_uuid_dict <- function(uuid) {
  paste0(
    "https://data.cms.gov/",
    "provider-data/dataset/",
    uuid, "#data-dictionary")
}

#' Get Field Names and Number of Rows from Provider Endpoint
#' @param uuid `<chr>` endpoint UUID
#' @returns `<list>` number of rows and field names
#' @autoglobal
#' @keywords internal
#' @export
prov_nrows_fields <- function(uuid) {

  x <- uuid |>
    prov_uuid_url() |>
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
       pages  = offset_length(x$count, 2000L))

}

#' Load `<CurrentProvider>` API Endpoint
#'
#' @param alias `<chr>` endpoint alias
#'
#' @returns `<CurrentProvider>` object
#'
#' @examples
#' provider_current("affiliations")
#' provider_current("clinicians")
#' provider_current("utilization")
#' @autoglobal
#'
#' @export
provider_current <- function(alias) {

  x <- catalog_provider()$doctors_and_clinicians |>
    subset_detect(
      title,
      alias_provider_current(alias)) |>
    c()

  q <- prov_nrows_fields(x$identifier)

  CurrentProvider(
    title       = x$title,
    description = x$description,
    contact     = x$contact,
    modified    = x$modified,
    uuid        = x$identifier,
    download    = x$download,
    issued      = x$issued,
    released    = x$released,
    site        = x$site,
    rows        = q$rows,
    fields      = q$fields,
    pages       = q$pages
  )
}

#' Load `<CurrentProviderGroup>` API Endpoint
#' @param alias `<chr>` endpoint alias
#' @returns `<CurrentProviderGroup>` object
#' @examples
#' provider_current_group("mips")
#' @autoglobal
#' @export
provider_current_group <- function(alias) {

  catalog_provider()$doctors_and_clinicians |>
    subset_detect(
      title,
      alias_provider_current_group(alias))
}
