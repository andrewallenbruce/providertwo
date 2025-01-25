#' CMS Public Catalog
#'
#' @returns `<list>` of `<tibbles>`: `dataset`, `distribution`, `downloads`
#'
#' @examples
#' Catalog_public()
#'
#' @autoglobal
#'
#' @export
Catalog_public <- \() {

  dataset <- qTBL(gelm(fload("https://data.cms.gov/data.json"), "dataset")) |>
    mtt(modified           = as_date(modified),
        accrualPeriodicity = recode_iso8601(accrualPeriodicity),
        keyword            = flatten_column(keyword),
        description        = sf_remove(description, "\n"),
        bureauCode         = delist(bureauCode),
        programCode        = delist(programCode),
        references         = delist(references))

  distribution <- qTBL(
    rowbind(gelm(dataset, "distribution"), fill = TRUE)) |>
    mtt(modified = as_date(modified))

  list(
    dataset = slt(
      dataset,
      title,
      accrualPeriodicity,
      contactPoint,
      describedBy,
      description,
      identifier,
      keyword,
      landingPage,
      modified,
      references,
      temporal),
    distribution = sbt(
      distribution,
      not_na(format) & na(description),
      title,
      modified,
      temporal,
      accessURL,
      resourcesAPI),
    downloads = sbt(
      distribution,
      mediaType %==% "text/csv",
      title,
      modified,
      temporal,
      downloadURL,
      resourcesAPI)
    )
}

#' CMS Provider Catalog
#'
#' @returns `<list>` of `<tibbles>`: `dataset`, `distribution`
#'
#' @examples
#' Catalog_provider()
#'
#' @autoglobal
#'
#' @export
Catalog_provider <- \() {

  dataset <- qTBL(
    fload("https://data.cms.gov/provider-data/api/1/metastore/schemas/dataset/items")) |>
    sbt(sf_ndetect(title, "Office Visit Costs$")) |>
    mtt(issued      = as_date(modified),
        modified    = as_date(modified),
        released    = as_date(modified),
        keyword     = flatten_column(keyword),
        description = sf_remove(description, "\n"),
        bureauCode  = delist(bureauCode),
        programCode = delist(programCode))

  distribution <- qTBL(
    rowbind(gelm(dataset, "distribution"), fill = TRUE)) |>
    add_vars(gelm(dataset, "title"), pos = "front")

  list(
    dataset = slt(
      dataset,
      landingPage,
      issued,
      modified,
      released,
      keyword,
      contactPoint,
      identifier,
      description,
      title),
    distribution = slt(
      distribution,
      title,
      downloadURL)
  )
}
