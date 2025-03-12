#' CMS Provider Catalog
#'
#' @returns `<list>` of Provider API catalog information
#'
#' @examples
#' catalog_provider()
#'
#' @autoglobal
#'
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
      downloadurl,
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
#' @param x `<chr>` UUID
#' @returns `<chr>` URL
#' @export
#' @autoglobal
#' @keywords internal
prov_uuid_url <- function(x) {
  paste0(
    "https://data.cms.gov/",
    "provider-data/api/1/",
    "datastore/query/",
    x, "/0")
}

#' Convert Provider UUID to Data Dictionary Hyperlink
#' @param x `<chr>` UUID
#' @returns `<chr>` URL
#' @export
#' @autoglobal
#' @keywords internal
prov_uuid_dict <- function(x) {
  paste0(
    "https://data.cms.gov/",
    "provider-data/dataset/",
    x, "#data-dictionary")
}

#' Get Field Names and Number of Rows from Provider Endpoint
#' @param url `<chr>` Endpoint UUID
#' @returns `<list>` of number of rows and field names
#' @autoglobal
#' @keywords internal
#' @export
prov_nrows_fields <- function(url) {

  x <- url |>
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

  list(rows = x$count, fields = x$query$properties)

}

#' Load `CurrentProvider` API Endpoint
#'
#' @param alias `<chr>` endpoint alias
#'
#' @returns `<CurrentProvider>` object
#'
#' @examples
#' provider_dataset("affiliations")
#' provider_dataset("clinicians")
#' provider_dataset("utilization")
#' provider_dataset("utilization")
#' provider_dataset("group_mips")
#' provider_dataset("group_patient")
#' provider_dataset("clin_mips")
#' provider_dataset("clin_overall")
#' provider_dataset("vgroup_mips")
#' @autoglobal
#'
#' @export
provider_dataset <- function(alias) {

  x <- catalog_provider()$doctors_and_clinicians |>
    subset_detect(
      title,
      alias_provider(alias)) |>
    c()

  q <- prov_nrows_fields(x$identifier)

  CurrentProvider(
    title       = x$title,
    description = x$description,
    contact     = x$contact,
    modified    = x$modified,
    identifier  = x$identifier,
    download    = x$downloadurl,
    issued      = x$issued,
    released    = x$released,
    site        = x$site,
    rows        = q$rows,
    fields      = q$fields
  )
}
