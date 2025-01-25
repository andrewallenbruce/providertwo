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
      modified,
      accrualPeriodicity,
      temporal,
      description,
      keyword,
      identifier,
      contactPoint,
      describedBy,
      references,
      landingPage),
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
      downloadURL)
    )
}

#' CMS Provider Catalog
#'
#' @returns `<list>` of `<tibbles>`: `dataset`, `download`
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
    mtt(issued      = as_date(issued),
        modified    = as_date(modified),
        released    = as_date(released),
        keyword     = flatten_column(keyword),
        description = sf_remove(description, "\n"))

  download <- qTBL(
    rowbind(gelm(dataset, "distribution"), fill = TRUE)) |>
    add_vars(gelm(dataset, "title"), pos = "front")

  list(
    dataset = slt(
      dataset,
      title,
      issued,
      modified,
      released,
      description,
      identifier,
      contactPoint,
      keyword,
      landingPage),
    download = slt(
      download,
      title,
      downloadURL)
  )
}

#' CMS Open Payments Catalog
#'
#' @returns `<list>` of `<tibbles>`: `dataset`, `distribution`, `downloads`
#'
#' @examplesIf FALSE
#' Catalog_openpayments()
#'
#' @autoglobal
#'
#' @export
Catalog_openpayments <- \() {

  dataset <- qTBL(fload("https://openpaymentsdata.cms.gov/api/1/metastore/schemas/dataset/items?show-reference-ids"), keep.attr = TRUE) |>
    mtt(issued             = as_date(issued),
        modified           = as_date(modified),
        accrualPeriodicity = recode_iso8601(accrualPeriodicity),
        description        = sf_remove(description, "\n"),
        bureauCode         = delist(bureauCode),
        programCode        = delist(programCode))

  identifiers <- uniq(qTBL(rowbind(gelm(dataset, "keyword"))))



  identifier <- qTBL(rowbind(gelm(dataset, "distribution"), fill = TRUE), keep.attr = TRUE)

  distribution <- qTBL(rowbind(gelm(identifier, "data"), fill = TRUE), keep.attr = TRUE)

  dataset <- slt(dataset, -distribution) |>
    add_vars(id2 = gelm(identifier, "identifier"))

  list(
    dataset = slt(
      dataset,
      title,
      modified,
      accrualPeriodicity,
      temporal,
      description,
      keyword,
      identifier,
      contactPoint,
      describedBy,
      references,
      landingPage),
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
      downloadURL)
  )
}
