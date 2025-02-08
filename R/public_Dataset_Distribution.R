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

  if (!exists(".__provider")) .__provider <<- Catalog_provider()

  dataset <- if (fname) fname_to_dataset(dataset) else dataset

  x <- c(sbt(.__provider, sf_detect(title, dataset)))

  Dataset(
    contact     = Contact(getElement(x$contactPoint[[1]], "fn"),
                          getElement(x$contactPoint[[1]], "hasEmail")),
    identifier  = Identifier(x$identifier),
    modified    = x$modified,
    title       = x$title,
    # periodicity = x$accrualPeriodicity,
    dictionary  = x$describedBy,
    description = x$description,
    keyword     = x$keyword,
    landingpage = x$landingPage,
    # references  = x$references,
    # downloadURL  = x$downloadURL,
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
public_Dataset <- function(dataset, fname = TRUE) {

  if (!exists(".__public")) .__public <<- Catalog_public()

  dataset <- if (fname) fname_to_dataset(dataset) else dataset

  x <- c(as.list(sbt(.__public$dataset, sf_detect(title, dataset))),
         as.list(sbt(.__public$distribution, sf_detect(title, dataset))[1, 5]))

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

  if (!exists(".__public")) .__public <<- Catalog_public()

  dataset <- if (fname) fname_to_dataset(dataset) else dataset

  x <- c(as.list(sbt(.__public$dataset, sf_detect(title, dataset))),
         as.list(sbt(.__public$distribution, sf_detect(title, dataset))[1, 5]),
         list(distribution = sbt(.__public$distribution, sf_detect(title, dataset))))

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
