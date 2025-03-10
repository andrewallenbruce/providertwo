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

  x <- fload(
    "https://data.cms.gov/data.json",
    query = "/dataset") |>
    mtt(
      modified    = as_date(modified),
      periodicity = recode_iso8601(accrualPeriodicity),
      references  = delist(references),
      description = replace_fixed(description,
        c("\n", "\r. \r.",'"', paste0(
          "<p><strong>NOTE: ",
          "</strong>This is a very large file and, ",
          "depending on your network characteristics and software, ",
          "may take a long time to download or fail to download. ",
          "Additionally, the number of rows in the file may be larger ",
          "than the maximum rows your version of <a href=\"https://support.",
          "microsoft.com/en-us/office/excel-specifications-and-limits-",
          "1672b34d-7043-467e-8e27-269d656771c3\">Microsoft Excel</a> supports. ",
          "If you can't download the file, we recommend engaging your IT support staff. ",
          "If you are able to download the file but are unable to open it in MS Excel or ",
          "get a message that the data has been truncated, we recommend trying alternative ",
          "programs such as MS Access, Universal Viewer, Editpad or any other software your ",
          "organization has available for large datasets.</p>")
          ),
        c(". ", "", "", "")
        )) |>
    as_tbl()

  x <- x |>
    mtt(contact = as.character(
      glue(
        '{delist(get_elem(x$contactPoint, "fn"))} ',
           '(',
           '{delist(get_elem(x$contactPoint, "^has", regex = TRUE))}',
           ')'
        ))) |>
    slt(
      title,
      description,
      modified,
      periodicity,
      temporal,
      contact,
      dictionary = describedBy,
      distribution,
      identifier,
      site = landingPage,
      references)

  d <- get_elem(x, "distribution") |> rowbind(fill = TRUE) |> as_tbl() |>
    mtt(modified    = as_date(modified),
        format      = cheapr_if_else(not_na(description), paste0(format, "-", description), format),
        `@type`     = NULL,
        description = NULL) |>
    colorder(title)

  list(
    dataset = slt(x, title, description, periodicity, contact, dictionary, identifier, modified, site, temporal, references) |> uniq(),
    download = sbt(d, not_na(mediaType), title, mediaType, downloadURL, modified, temporal) |> uniq(),
    distribution = sbt(d, not_na(format), title, format, accessURL, modified, temporal) |> uniq(),
    resources = slt(d, title, resourcesAPI, modified, temporal) |> uniq()
  )
}

#' Load Main API `Dataset`
#' @param alias `<chr>` endpoint alias
#' @returns `<Dataset>` object
#' @examples
#' main_current("enrollees")
#' main_current("hospitals")
#' main_current("reassignments")
#' main_current("opt_out")
#' main_current("laboratories")
#' @autoglobal
#' @export
main_current <- function(alias) {

  c(subset_detect(
    catalog_main()$dataset,
    title,
    alias_main_current(alias)
  ))
}

#' Load Public API `Distribution`
#' @param alias `<chr>` dataset title
#' @returns `<Distribution>` object
#' @examples
#' main_temporal("utilization_provider")
#' main_temporal("quality_payment")
#' @autoglobal
#' @export
main_temporal <- function(alias) {
  c(subset_detect(
    get_elem(catalog_main(), "distribution"),
    title,
    alias_main_temporal(alias)
  ))

}
