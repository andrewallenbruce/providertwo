#' Public CMS Dataset Object
#'
#' @autoglobal
#'
#' @returns `<list>` of `<tibbles>`: `dataset`, `latest`, `api`, `csv`
#'
#' @export
public_dataset <- \() {

  dataset <- RcppSimdJson::fload("https://data.cms.gov/data.json")

  dataset <- collapse::qTBL(dataset[["dataset"]])

  distribution <- collapse::slt(dataset, distribution) |>
    tidyr::unnest(distribution)

  list(
    dataset = collapse::slt(dataset, title, modified, temporal, accrualPeriodicity, identifier, describedBy, description, landingPage),
    latest  = collapse::sbt(distribution, description %==% "latest", title, modified, temporal, accessURL, resourcesAPI),
    api     = collapse::sbt(distribution, not_na(format) & na(description), title, modified, temporal, accessURL, resourcesAPI),
    csv     = collapse::sbt(distribution, mediaType %==% "text/csv", title, modified, temporal, downloadURL, resourcesAPI)
  )
}

#' Filter Public Catalog
#'
#' @param endpoint `<chr>` API endpoint; options are `"dataset"` (default),
#'   `"latest"`, `"api"`, `"csv"`
#'
#' @param title `<chr>` dataset title to search for; use `NULL` to return all
#'
#' @returns `<tibble>` of search results
#'
#' @examples
#' public_filter(
#'   endpoint = "latest",
#'   title = "Medicare Fee-For-Service Public Provider Enrollment : 2024-09-01")
#'
#' public_filter(
#'   endpoint = "dataset",
#'   title = "Medicare Fee-For-Service  Public Provider Enrollment")
#'
#' @autoglobal
#'
#' @export
public_filter <- function(endpoint = c("dataset", "latest", "api", "csv"),
                          title = NULL) {

  endpoint <- match.arg(endpoint)

  if (!exists(".__public")) .__public <<- public_dataset()

  x <- .__public[[endpoint]]

  search_in(x, "title", title)
}
