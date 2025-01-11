#' Public CMS Dataset Object
#'
#' @returns `<list>` of `<tibbles>`: `dataset`, `latest`, `api`, `csv`
#'
#' @examples
#' public_dataset()
#'
#' @autoglobal
#'
#' @export
public_dataset <- \() {

  dataset <- fload("https://data.cms.gov/data.json")

  dataset <- qTBL(dataset[["dataset"]]) |>
    mtt(modified = as_date(modified),
        accrualPeriodicity = recode_iso8601(accrualPeriodicity))
      # temporal = as_date(sf_sub(temporal, stop = 10)),

  distribution <- slt(dataset, distribution) |>
    unnest(distribution) |>
    mtt(modified = as_date(modified))
      # temporal = as_date(sf_sub(temporal, start = 12, stop = 21)),

  list(
    dataset = slt(dataset, title, modified, temporal, accrualPeriodicity, identifier, describedBy, description, landingPage),
    api = sbt(distribution, not_na(format) & na(description), title, modified, temporal, accessURL, resourcesAPI),
    csv = sbt(distribution, mediaType %==% "text/csv", title, modified, temporal, downloadURL, resourcesAPI)
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
#'   endpoint = "dataset",
#'   title    = "Medicare Fee-For-Service  Public Provider Enrollment")
#'
#' public_filter(
#'   endpoint = "api",
#'   title    = "Medicare Fee-For-Service Public Provider Enrollment : 2024-09-01")
#'
#' public_filter(
#'   endpoint = "csv",
#'   title    = "Medicare Fee-For-Service Public Provider Enrollment : 2024-09-01")
#'
#' @autoglobal
#'
#' @export
public_filter <- \(endpoint = c("dataset", "api", "csv"), title = NULL) {

  endpoint <- match.arg(endpoint)

  if (!exists(".__public")) .__public <<- public_dataset()

  x <- .__public[[endpoint]]

  search_in(x, "title", title)
}
