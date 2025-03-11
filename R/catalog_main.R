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
catalog_main <- function() {

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

#' CMS Main Catalog
#'
#' @returns `<list>` of CMS Main API catalog information
#'
#' @examples
#' catalog_main2()
#'
#' @autoglobal
#'
#' @export
catalog_main2 <- function() {

  x <- fload("https://data.cms.gov/data.json", query = "/dataset")

  x <- mtt(x,
      modified    = as_date(modified),
      periodicity = recode_iso8601(accrualPeriodicity),
      references  = delist(references),
      title       = gsub("  ", " ", title, perl = TRUE),
      contact     = as.character(glue('{delist(get_elem(x$contactPoint, "fn"))} ({delist(get_elem(x$contactPoint, "^has", regex = TRUE))})')),
      description = sf_remove(description, "[\"']"),
      description = sf_remove(description, "Note: This full dataset contains more records than most spreadsheet programs can handle, which will result in an incomplete load of data. Use of a database or statistical software is required.$"),
      description = sf_remove(description, "^ATTENTION USERSSome Providers Opt-Out Status may end early due to COVID 19 waivers. Please contact your respective MAC for further information. For more information on the opt-out process, see Manage Your Enrollment or view the FAQ section below. $"),
      description = stri_trim(description)) |>
    slt(
      title,
      description,
      modified,
      periodicity,
      temporal,
      contact,
      identifier,
      dictionary  = describedBy,
      site        = landingPage,
      references,
      distribution) |>
    as_tbl()

  d <- get_elem(x, "distribution") |> rowbind(fill = TRUE) |> as_tbl() |>
    mtt(
      modified     = as_date(modified),
      format       = cheapr_if_else(not_na(description), paste0(format, "-", description), format),
      `@type`      = NULL,
      description  = NULL,
      year         = as_int(stri_extract_all_regex(title, "[0-9]{4}")),
      title        = stri_replace_all_regex(title, " : [0-9]{4}-[0-9]{2}-[0-9]{2}([0-9A-Za-z]{1,3})?$", ""),
      download     = lag_(downloadURL, n = -1L),
      filetype     = lag_(mediaType, n = -1L),
      resources    = resourcesAPI,
      resourcesAPI = NULL,
      downloadURL  = NULL,
      mediaType    = NULL) |>
    colorder(title)

  d <- funique(sset(d, row_na_counts(d) < 4), cols = c("title", "year"))

  out <- sbt(fcount(d, title), N == 1) |> _[["title"]]

  mn <- join(
    slt(
      x,
      title,
      description,
      periodicity,
      contact,
      dictionary,
      identifier,
      modified,
      site,
      temporal,
      references
    ),
    sbt(
      d,
      format == "API-latest",
      -format,
      -accessURL,
      -modified,
      -temporal,
      -year
    ),
    on = "title",
    verbose = 0
  ) |> roworder(title)

  tmp <- join(
    sbt(d, !title %in% out) |>
      slt(year, title, identifier = accessURL, modified:resources) |>
      roworder(title, -year) |>
      f_nest_by(.cols = "title") |>
      f_ungroup(),
    slt(mn, title, description, periodicity, contact, dictionary, site),
    on = "title",
    verbose = 0
  )

  list(
    current = mn,
    temporal = tmp
    )
}

#' Load Main API `Dataset`
#' @param alias `<chr>` endpoint alias
#' @returns `<Dataset>` object
#' @examples
#' main_current("enrollees")
#' main_current("opt_out")
#' main_current("order_refer")
#' main_current("reassignments")
#' main_current("hospitals")
#' main_current("laboratories")
#' main_current("crosswalk")
#' main_current("rbcs")
#' main_current("rhc")
#' main_current("fqhc")
#' main_current("home_health")
#' main_current("hospice")
#' main_current("snf")
#' main_current("pending_nonphysicians")
#' main_current("pending_physicians")
#' @autoglobal
#' @export
main_current <- function(alias) {
  catalog_main()$dataset |>
    subset_detect(title, alias_main_current(alias)) |>
    c()
}

#' Load Public API `Distribution`
#' @param alias `<chr>` dataset title
#' @returns `<Distribution>` object
#' @examples
#' main_temporal("quality_payment")
#'
#' main_temporal("utilization_provider")
#' main_temporal("utilization_provider")
#' main_temporal("utilization_provider")
#'
#' main_temporal("prescribers_drug")
#' main_temporal("prescribers_provider")
#' main_temporal("prescribers_geography")
#'
#' @autoglobal
#' @export
main_temporal <- function(alias) {
  catalog_main()$distribution |>
    subset_detect(title, alias_main_temporal(alias)) |>
    c()

}
