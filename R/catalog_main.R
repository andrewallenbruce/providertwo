#' CMS Main Catalog
#'
#' @returns `<list>` of CMS Main API catalog information
#'
#' @examples
#' catalog_main()
#'
#' @autoglobal
#'
#' @export
catalog_main <- \() {

  dataset <- as_tbl(
    fload("https://data.cms.gov/data.json", query = "/dataset") |>
      slt(
        accrualPeriodicity,
        contactPoint,
        describedBy,
        description,
        distribution,
        identifier,
        keyword,
        landingPage,
        modified,
        references,
        temporal,
        theme,
        title
      ) |>
      mtt(
        modified           = as_date(modified),
        accrualPeriodicity = recode_iso8601(accrualPeriodicity),
        keyword            = flatten_column(keyword),
        theme              = flatten_column(theme),
        description        = trimws(sf_remove(description, "\n")),
        references         = delist(references)
      )
  )

  distribution <- as_tbl(rowbind(get_elem(dataset, "distribution"), fill = TRUE)) |>
    mtt(modified    = as_date(modified),
        format      = cheapr_if_else(not_na(description), paste0(format, "-", description), format),
        `@type`     = NULL,
        description = NULL) |>
    colorder(title)

  list(
    dataset = slt(dataset, title, theme, keyword, description, accrualPeriodicity, contactPoint, describedBy, identifier, modified, landingPage, temporal, references) |> uniq(),
    download = sbt(distribution, not_na(mediaType), title, mediaType, downloadURL, modified, temporal) |> uniq(),
    distribution = sbt(distribution, not_na(format), title, format, accessURL, modified, temporal) |> uniq(),
    resources = slt(distribution, title, resourcesAPI, modified, temporal) |> uniq()
  )
}

#' Load Main API `Dataset`
#'
#' @param dataset `<chr>` dataset title
#'
#' @returns `<Dataset>` object
#'
#' @examples
#' main_dataset("enrollees")
#'
#' main_dataset("hospitals")
#'
#' main_dataset("reassignments")
#'
#' main_dataset("opt_out")
#'
#' main_dataset("laboratories")
#'
#' @autoglobal
#'
#' @export
main_dataset <- function(dataset) {
  c(subset_detect(
    get_elem(catalog_main(), "dataset"),
    title,
    fname_to_dataset(dataset)
  ))
}

#' Load Public API `Distribution`
#'
#' @param dataset `<chr>` dataset title
#'
#' @returns `<Distribution>` object
#'
#' @examples
#' main_temporal("utilization_provider")
#'
#' main_temporal("quality_payment")
#'
#' @autoglobal
#'
#' @export
main_temporal <- function(dataset) {
  c(subset_detect(
    get_elem(catalog_main(), "distribution"),
    title,
    fname_to_dataset(dataset)
  ))

}
