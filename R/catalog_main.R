#' Resources Class
#' @param url `<chr>` resourcesAPI URL
#' @returns `<S7_class>` class_resources object
#' @family classes
#' @autoglobal
#' @export
class_resources <- new_class(
  name       = "class_resources",
  properties = list(
    url      = class_character,
    files    = new_property(
      class  = class_list,
      getter = function(self)
        fload(self@url, query = "/data") |>
        as_df() |>
        fcompute(
          file         = name,
          size         = roundup(fileSize / 1e6),
          ext          = file_ext(downloadURL),
          downloadurl  = downloadURL) |>
        roworder(ext, -size)
    )
  ),
  validator = function(self)
    if (length(self@url) != 1L) "must be length 1"
)

#' Load Public API `Dataset`
#'
#' @param dataset `<chr>` dataset title
#'
#' @param fname `<lgl>` Is `dataset` a function name?; default is `TRUE`
#'
#' @returns `<Dataset>` object
#'
#' @examples
#' public_dataset("enrollees")
#'
#' public_dataset("hospitals")
#'
#' public_dataset("reassignments")
#'
#' public_dataset("opt_out")
#'
#' public_dataset("laboratories")
#'
#' @autoglobal
#'
#' @export
public_dataset <- function(dataset = "enrollees", fname = TRUE) {

  if (!exists(".api__public")) .api__public <<- load_public()

  dataset <- if (fname) fname_to_dataset(dataset) else dataset

  x <- c(
    sbt(.api__public$dataset, sf_detect(title, dataset)),
    sset(sbt(.api__public$resources, sf_detect(title, dataset)), 1, j = "resourcesAPI")
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
#' public_temporal("utilization_provider")
#'
#' public_temporal("quality_payment")
#'
#' @autoglobal
#'
#' @export
public_temporal <- function(dataset, fname = TRUE) {

  if (!exists(".api__public")) .api__public <<- load_public()

  dataset <- if (fname) fname_to_dataset(dataset) else dataset

  x <- c(sbt(.api__public$dataset, sf_detect(title, dataset)),
         sset(sbt(.api__public$resources, sf_detect(title, dataset)), 1, j = "resourcesAPI"),
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
