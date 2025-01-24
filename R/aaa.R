#' Public CMS Dataset Object
#'
#' @returns `<list>` of `<tibbles>`: `dataset`, `distribution`, `downloads`
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
    mtt(modified           = as_date(modified),
        accrualPeriodicity = recode_iso8601(accrualPeriodicity),
        keyword            = flatten_column(keyword),
        description        = sf_remove(description, "\n"))

  distribution <- slt(dataset, distribution) |>
    unnest(distribution) |>
    mtt(modified = as_date(modified))

  dataset <- slt(dataset, -distribution) |>
    unnest(bureauCode) |>
    unnest(programCode) |>
    unnest(references)

  list(
    dataset = slt(
      dataset,
      type = `@type`,
      accessLevel,
      accrualPeriodicity,
      bureauCode,
      contactPoint,
      describedBy,
      description,
      identifier,
      keyword,
      landingPage,
      modified,
      programCode,
      publisher,
      references,
      temporal,
      title),
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

#' Provider CMS Dataset Object
#'
#' @returns `<list>` of `<tibbles>`: `dataset`, `distribution`
#'
#' @examples
#' provider_dataset()
#'
#' @autoglobal
#'
#' @export
provider_dataset <- \() {

  dataset <- qTBL(fload("https://data.cms.gov/provider-data/api/1/metastore/schemas/dataset/items")) |>
    sbt(sf_ndetect(title, "Office Visit Costs$")) |>
    mtt(issued = as_date(modified),
        modified = as_date(modified),
        released = as_date(modified),
        keyword = flatten_column(keyword),
        description = sf_remove(description, "\n"))

  distribution <- slt(dataset, identifier, distribution) |>
    unnest(distribution)

  dataset <- slt(dataset, -distribution) |>
    unnest(bureauCode) |>
    unnest(programCode)

  list(
    dataset = slt(
      dataset,
      accessLevel,
      landingPage,
      bureauCode,
      issued,
      type = `@type`,
      modified,
      released,
      keyword,
      contactPoint,
      publisher,
      identifier,
      description,
      title,
      programCode),
    distribution = slt(
      distribution,
      identifier,
      type = `@type`,
      downloadURL,
      mediaType)
  )
}
