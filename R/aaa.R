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
    slt(title, accrualPeriodicity, contactPoint,
        describedBy,
        description,
        distribution,
        identifier,
        keyword,
        landingPage,
        modified,
        references,
        temporal,
        theme) |>
    mtt(modified           = as_date(modified),
        accrualPeriodicity = recode_iso8601(accrualPeriodicity),
        keyword            = flatten_column(keyword),
        theme              = flatten_column(theme),
        description        = sf_remove(description, "\n"),
        references         = delist(references))

  distribution <- qTBL(
    rowbind(gelm(dataset, "distribution"), fill = TRUE)) |>
    mtt(modified = as_date(modified))

  list(
    dataset      = slt(dataset, -distribution),
    distribution = join(
      sbt(distribution, not_na(format) & na(description), title, modified, temporal, accessURL, resourcesAPI),
      sbt(distribution, mediaType %==% "text/csv", title, temporal, downloadURL),
      on = c("title", "temporal"),
      verbose = 0)
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
    sbt(
      sf_ndetect(title, "Office Visit Costs$"),
      -`@type`,
      -accessLevel,
      -bureauCode,
      -programCode,
      -archiveExclude,
      -publisher) |>
    mtt(issued      = as_date(issued),
        modified    = as_date(modified),
        released    = as_date(released),
        keyword     = flatten_column(keyword),
        theme       = flatten_column(theme),
        description = sf_remove(description, "\n"),
        identifier  = paste0(
          "https://data.cms.gov/provider-data/api/1/datastore/query/",
          identifier,
          "/0"))

  download <- qTBL(
    rowbind(gelm(dataset, "distribution"), fill = TRUE)) |>
    add_vars(gelm(dataset, "title"), pos = "front") |>
    slt(-`@type`)

  join(dataset, download, on = "title", verbose = 0)
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

  dataset <- qTBL(
    fload("https://openpaymentsdata.cms.gov/api/1/metastore/schemas/dataset/items?show-reference-ids"),
    keep.attr = TRUE) |>
    mtt(issued             = as_date(issued),
        modified           = as_date(modified),
        accrualPeriodicity = recode_iso8601(accrualPeriodicity),
        description        = sf_remove(description, "\n"),
        bureauCode         = delist(bureauCode),
        programCode        = delist(programCode),
        theme              = delist(map(theme, \(x) gelm(as.list(x), "data"))),
        year               = delist(map(keyword, \(x) gelm(as.list(x), "data"))),
        modified_dttm      = as_datetime(`%modified`))

  distribution <- qTBL(rowbind(gelm(identifier, "data"), fill = TRUE), keep.attr = TRUE)

  list(
    dataset = slt(
      dataset,
      year,
      title,
      description,
      theme,
      issued,
      modified,
      modified_dttm,
      temporal,
      accrualPeriodicity,
      distribution),
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
