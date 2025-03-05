#' Load Provider API `Dataset`
#'
#' @param dataset `<chr>` dataset title
#'
#' @param fname `<lgl>` Is `dataset` a function name?; default is `TRUE`
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
provider_dataset <- function(dataset, fname = TRUE) {


  if (!exists(".api__provider")) .api__provider <<- load_provider()

  dataset <- if (fname) fname_to_dataset(dataset) else dataset

  x <- c(sbt(.api__provider, sf_detect(title, dataset)))

  Dataset(
    contact     = Contact(getElement(x$contactPoint[[1]], "fn"),
                          getElement(x$contactPoint[[1]], "hasEmail")),
    identifier  = Identifier(x$identifier),
    modified    = x$modified,
    title       = x$title,
    dictionary  = x$describedBy,
    description = x$description,
    keyword     = x$keyword,
    landingpage = x$landingPage,
    temporal    = paste0(x$issued, "/", x$released),
    theme       = x$theme
  )
}

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
      "metastore/schemas/dataset/items")) |>
    as_tbl() |>
      mtt(
        issued      = as_date(issued),
        modified    = as_date(modified),
        released    = as_date(released),
        keyword     = flatten_column(keyword),
        theme       = flatten_column(theme),
        description = trimws(sf_remove(description, "\n")))

  add_vars(x,
    downloadurl = delist(
      get_elem(x$distribution,
               "downloadURL",
               DF.as.list = TRUE)),
    contact = as.character(
             glue(
               '{delist(get_elem(x$contactPoint, "fn"))} ',
               '({delist(get_elem(x$contactPoint, "^has", regex = TRUE))})'
             ))) |>
    slt(
      title,
      theme,
      description,
      keyword,
      issued,
      modified,
      released,
      identifier,
      contact,
      downloadurl,
      site = landingPage
    ) |>
    roworder(theme, title) |>
    rsplit( ~ theme) |>
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
