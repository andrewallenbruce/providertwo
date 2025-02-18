#' Load Open Payments API `Dataset`
#'
#' @param period `<chr>` dataset year
#'
#' @param group `<chr>` dataset group keyword
#'
#' @param dataset `<chr>` dataset title
#'
#' @returns `<Dataset>` object
#'
#' @examples
#' open_Dataset(period = "All", group = "Summary", dataset = "Covered Recipient Profile Supplement")
#'
#' @autoglobal
#'
#' @export
open_Dataset <- function(period = NULL, group = NULL, dataset = NULL) {


  if (!exists(".api__openpay")) .api__openpay <<- load_openpayments()

  if (not_null(period))   x <- sbt(.api__openpay, sf_detect(year, period))
  if (not_null(group))    x <- sbt(.api__openpay, sf_detect(theme, group))
  if (not_null(dataset))  x <- sbt(.api__openpay, sf_detect(title, dataset))

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

#' Load Provider API `Dataset`
#'
#' @param dataset `<chr>` dataset title
#'
#' @param fname `<lgl>` Is `dataset` a function name?; default is `TRUE`
#'
#' @returns `<Dataset>` object
#'
#' @examples
#' provider_Dataset("affiliations")
#'
#' provider_Dataset("clinicians")
#'
#' @autoglobal
#'
#' @export
provider_Dataset <- function(dataset, fname = TRUE) {


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

#' Load Public API `Dataset`
#'
#' @param dataset `<chr>` dataset title
#'
#' @param fname `<lgl>` Is `dataset` a function name?; default is `TRUE`
#'
#' @returns `<Dataset>` object
#'
#' @examples
#' public_Dataset("enrollees")
#'
#' public_Dataset("hospitals")
#'
#' public_Dataset("reassignments")
#'
#' public_Dataset("opt_out")
#'
#' public_Dataset("laboratories")
#'
#' @autoglobal
#'
#' @export
public_Dataset <- function(dataset = "enrollees", fname = TRUE) {

  if (!exists(".api__public")) .api__public <<- load_public()

  dataset <- if (fname) fname_to_dataset(dataset) else dataset

  x <- c(as.list(sbt(.api__public$dataset, sf_detect(title, dataset))),
         as.list(sset(sbt(.api__public$distribution, sf_detect(title, dataset)), 1, j = "resourcesAPI"))
         )

  Dataset(
    contact     = Contact(getElement(x$contactPoint[[1]], "fn"),
                          getElement(x$contactPoint[[1]], "hasEmail")),
    identifier  = Identifier(x$identifier),
    resources   = Resources(x$resourcesAPI),
    modified    = x$modified,
    title       = x$title,
    periodicity = x$accrualPeriodicity,
    dictionary  = x$describedBy,
    description = x$description,
    keyword     = x$keyword,
    landingpage = x$landingPage,
    references  = x$references,
    temporal    = x$temporal,
    theme       = x$theme
  )

}

#' Load Public API `Distribution`
#'
#' @param dataset `<chr>` dataset title
#'
#' @param fname `<lgl>` Is `dataset` a function name?; default is `TRUE`
#'
#' @returns `<Distribution>` object
#'
#' @examples
#' public_Distribution("utilization_provider")
#'
#' public_Distribution("quality_payment")
#'
#' @autoglobal
#'
#' @export
public_Distribution <- function(dataset, fname = TRUE) {

  if (!exists(".api__public")) .api__public <<- load_public()

  dataset <- if (fname) fname_to_dataset(dataset) else dataset

  x <- c(as.list(sbt(.api__public$dataset, sf_detect(title, dataset))),
         as.list(sset(sbt(.api__public$distribution, sf_detect(title, dataset)), 1, j = "resourcesAPI")),
         list(distribution = sbt(.api__public$distribution, sf_detect(title, dataset))))

  Distribution(
    contact       = Contact(getElement(x$contactPoint[[1]], "fn"),
                            getElement(x$contactPoint[[1]], "hasEmail")),
    identifier    = Identifier(x$identifier),
    resources     = Resources(x$resourcesAPI),
    modified      = x$modified,
    title         = x$title,
    periodicity   = x$accrualPeriodicity,
    dictionary    = x$describedBy,
    description   = x$description,
    keyword       = x$keyword,
    landingpage   = x$landingPage,
    references    = x$references,
    temporal      = x$temporal,
    theme         = x$theme,
    distributions = x$distribution
  )

}
