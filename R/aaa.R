#' CMS Public Catalog
#'
#' @returns `<list>` of `<tibbles>`: `dataset`, `distribution`
#'
#' @examples
#' Catalog_public()
#'
#' @autoglobal
#'
#' @export
Catalog_public <- \() {

  dataset <- as_tbl(
    fload("https://data.cms.gov/data.json",
          query = "/dataset")) |>
    slt(title,
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
        theme) |>
    mtt(modified           = as_date(modified),
        accrualPeriodicity = recode_iso8601(accrualPeriodicity),
        keyword            = flatten_column(keyword),
        theme              = flatten_column(theme),
        description        = sf_remove(description, "\n"),
        references         = delist(references))

  distribution <- as_tbl(
    rowbind(gelm(dataset, "distribution"), fill = TRUE)) |>
    mtt(modified = as_date(modified))

  list(
    dataset      = slt(dataset, -distribution),
    distribution = join(
      sbt(distribution,
          not_na(format) & na(description),
          title, modified, temporal, accessURL, resourcesAPI),
      sbt(distribution,
          mediaType %==% "text/csv",
          title, temporal, downloadURL),
      on = c("title", "temporal"),
      verbose = 0)
    )
}

#' CMS Provider Catalog
#'
#' @returns `<tibble>` of Provider API catalog information
#'
#' @examples
#' Catalog_provider()
#'
#' @autoglobal
#'
#' @export
Catalog_provider <- \() {

  dataset <- as_tbl(
    fload("https://data.cms.gov/provider-data/api/1/metastore/schemas/dataset/items")) |>
    sbt(sf_ndetect(title, "Office Visit Costs$"),
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
        describedBy = paste0("https://data.cms.gov/provider-data/dataset/", identifier, "#data-dictionary"),
        identifier  = paste0("https://data.cms.gov/provider-data/api/1/datastore/query/", identifier, "/0"))

  download <- as_tbl(
    rowbind(gelm(dataset, "distribution"), fill = TRUE)) |>
    add_vars(gelm(dataset, "title"), pos = "front") |>
    slt(-`@type`,
        -mediaType)

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

  dataset <- as_tbl(
    fload("https://openpaymentsdata.cms.gov/api/1/metastore/schemas/dataset/items?show-reference-ids")) |>
    mtt(issued             = as_date(issued),
        modified           = as_date(modified),
        accrualPeriodicity = recode_iso8601(accrualPeriodicity),
        description        = sf_remove(description, "\n"),
        bureauCode         = delist(bureauCode),
        programCode        = delist(programCode),
        theme              = delist(map(theme, \(x) gelm(as.list(x), "data"))),
        year               = delist(map(keyword, \(x) gelm(as.list(x), "data"))),
        modified_dttm      = as_datetime(`%modified`))

  distribution <- as_tbl(rowbind(gelm(identifier, "data"), fill = TRUE))

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
