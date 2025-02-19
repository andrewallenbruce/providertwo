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
    mtt(
      modified    = as_date(modified),
      format      = cheapr_if_else(not_na(description), paste0(format, "-", description), format),
      `@type`     = NULL,
      description = NULL
    ) |>
    colorder(title)

  list(
    dataset      = slt(
      dataset,
      title,
      theme,
      keyword,
      description,
      accrualPeriodicity,
      contactPoint,
      describedBy,
      identifier,
      modified,
      landingPage,
      temporal,
      references
    ),
    distribution = join(
      sbt(
        distribution,
        not_na(mediaType),
        title,
        mediaType,
        downloadURL,
        resourcesAPI,
        modified,
        temporal
      ),
      sbt(
        distribution,
        not_na(format),
        title,
        format,
        accessURL,
        resourcesAPI,
        modified,
        temporal
      ),
      on = c("title", "temporal", "resourcesAPI", "modified"),
      verbose = 0,
      overid  = 0
    )
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
    fload(
      "https://data.cms.gov/provider-data/api/1/metastore/schemas/dataset/items"
    ) |>
      slt(
        -c(
          `@type`,
          accessLevel,
          bureauCode,
          programCode,
          archiveExclude,
          publisher
        )
      ) |>
      mtt(
        issued      = as_date(issued),
        modified    = as_date(modified),
        released    = as_date(released),
        keyword     = flatten_column(keyword),
        theme       = flatten_column(theme),
        description = trimws(sf_remove(description, "\n")),
        describedBy = paste0(
          "https://data.cms.gov/provider-data/dataset/",
          identifier,
          "#data-dictionary"
        ),
        identifier  = paste0(
          "https://data.cms.gov/provider-data/api/1/datastore/query/",
          identifier,
          "/0"
        )
      )
  )

  slt(dataset, -distribution) |>
    add_vars(downloadURL = delist(
      get_elem(dataset$distribution, "downloadURL", DF.as.list = TRUE)
    ))
}

#' CMS Open Payments Catalog
#'
#' @returns `<tibble>` of Open Payments API catalog information
#'
#' @examples
#' load_openpayments()
#'
#' @autoglobal
#'
#' @export
load_openpayments <- \() {

  dataset <- as_tbl(
    fload("https://openpaymentsdata.cms.gov/api/1/metastore/schemas/dataset/items?show-reference-ids") |>
      mtt(
        modified        = as_date(modified),
        description     = sf_replace(description, "\n", ". "),
        description     = sf_remove(description, "\r. \r."),
        theme           = delist(map(theme, \(x) get_elem(as.list(x), "data"))),
        year            = delist(map(keyword, \(x) get_elem(as.list(x), "data"))),
        year            = sf_replace(year, "all years", "All", fix = TRUE),
        identifier      = paste0("https://openpaymentsdata.cms.gov/api/1/datastore/query/", identifier, "/0")))

  describedby <- map(
    get_elem(
    get_elem(
      dataset$distribution, "data", DF.as.list = TRUE),
    "title|describedBy", regex = TRUE), \(x) x[not_null(names(x))])

  join(
    add_vars(dataset,
             downloadURL = delist(get_elem(get_elem(dataset$distribution, "data", DF.as.list = TRUE), "downloadURL"))),
    new_df(title = delist(get_elem(describedby, "title")),
           describedBy = delist(get_elem(describedby, "describedBy"))),
    on = "title",
    verbose = 0) |>
    slt(
      year,
      theme,
      title,
      description,
      modified,
      temporal,
      identifier,
      downloadURL,
      describedBy) |>
    roworder(-theme, -year, title)
}
