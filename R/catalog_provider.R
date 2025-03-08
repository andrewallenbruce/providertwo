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

#' Load Provider API `Dataset`
#'
#' @param dataset `<chr>` dataset title
#'
#' @returns `<Dataset>` object
#'
#' @examples
#' provider_dataset("affiliations")
#'
#' provider_dataset("clinicians")
#'
#' @autoglobal
#'
#' @export
provider_dataset <- function(dataset) {

  x <- c(subset_detect(
    get_elem(catalog_provider(), "doctors_and_clinicians"),
    title,
    fname_to_dataset(dataset)
  ))

  provider_current(
    title       = x$title,
    description = x$description,
    contact     = x$contact,
    modified    = x$modified,
    identifier  = prov_uuid_url(x$identifier),
    download    = x$downloadurl,
    issued      = x$issued,
    released    = x$released,
    site        = x$site
  )
}
