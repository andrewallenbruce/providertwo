#' CMS Public Catalog
#'
#' @returns `<list>` of `<tibbles>`: `dataset`, `distribution`
#'
#' @examples
#' load_public()
#'
#' @autoglobal
#'
#' @export
load_public <- \() {

  dataset <- as_tbl(
    fload("https://data.cms.gov/data.json",
          query = "/dataset") |>
    slt(accrualPeriodicity,
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
        title) |>
    mtt(modified           = as_date(modified),
        accrualPeriodicity = recode_iso8601(accrualPeriodicity),
        keyword            = flatten_column(keyword),
        theme              = flatten_column(theme),
        description        = trimws(sf_remove(description, "\n")),
        references         = delist(references)))

  distribution <- as_tbl(rowbind(
    get_elem(dataset, "distribution"),
    fill = TRUE)) |>
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
#' @returns `<tibble>` of Provider API catalog information
#'
#' @examples
#' load_provider()
#'
#' @autoglobal
#'
#' @export
load_provider <- \() {

  dataset <- as_tbl(
    fload("https://data.cms.gov/provider-data/api/1/metastore/schemas/dataset/items") |>
    slt(-c(`@type`, accessLevel, bureauCode, programCode, archiveExclude, publisher)) |>
    mtt(issued      = as_date(issued),
        modified    = as_date(modified),
        released    = as_date(released),
        keyword     = flatten_column(keyword),
        theme       = flatten_column(theme),
        description = trimws(sf_remove(description, "\n")),
        describedBy = paste0("https://data.cms.gov/provider-data/dataset/", identifier, "#data-dictionary"),
        identifier  = paste0("https://data.cms.gov/provider-data/api/1/datastore/query/", identifier, "/0")))

  dataset |>
    slt(-distribution) |>
    add_vars(
      downloadURL = delist(
        get_elem(
          dataset$distribution,
          "downloadURL",
          DF.as.list = TRUE)))
}

#' CMS Open Payments Catalog
#'
#' @returns `<list>` of `<tibbles>`: `dataset`, `distribution`, `downloads`
#'
#' @examples
#' load_openpayments()
#'
#' @autoglobal
#'
#' @export
load_openpayments <- \() {

  as_tbl(fload(
    "https://openpaymentsdata.cms.gov/api/1/metastore/schemas/dataset/items?show-reference-ids") |>
      mtt(issued          = as_date(issued),
          modified        = as_date(modified),
          description     = sf_remove(description, "\n"),
          theme           = delist(map(theme, \(x) gelm(as.list(x), "data"))),
          year            = delist(map(keyword, \(x) gelm(as.list(x), "data"))),
          identifier_year = delist(map(keyword, \(x) gelm(as.list(x), "identifier"))),
          identifier_dist = delist(map(distribution, \(x) gelm(as.list(x), "identifier"))),
          # distribution = delist(map(distribution, \(x) gelm(as.list(x), "data"))),
          modified_dttm   = as_datetime(`%modified`))) |>
    slt(
      theme,
      year,
      title,
      contactPoint,
      description,
      distribution,
      temporal,
      identifier,
      identifier_year,
      issued,
      modified,
      modified_dttm,
      publisher) |>
    unnest(distribution, names_sep = "_") |>
    unnest_wider(distribution_data, names_sep = "_") |>
    unnest_wider(`distribution_data_%Ref:downloadURL`, names_sep = "_") |>
    unnest_wider(`distribution_data_%Ref:downloadURL_data`, names_sep = "_") |>
    unnest_wider(`distribution_data_%Ref:downloadURL_data_1`, names_sep = "_")
}
