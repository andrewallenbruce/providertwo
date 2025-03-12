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

  mn <- join(slt(x, -distribution),
             sbt(d, format == "API-latest", -format, -accessURL, -modified, -temporal, -year),
             on = "title",
             verbose = 0) |>
    roworder(title)

  tmp <- join(
    sbt(d, !title %in% out, year, title, identifier = accessURL, modified:resources) |>
      roworder(title, -year) |>
      f_nest_by(.cols = "title") |>
      f_ungroup(),
    slt(mn, title, description, periodicity, contact, dictionary, site),
    on = "title",
    verbose = 0
  )

  list(current = mn, temporal = tmp)
}

#' Get Field Names and Number of Rows from Main Endpoint
#' @param url `<chr>` endpoint URL
#' @returns `<list>` number of rows and field names
#' @autoglobal
#' @keywords internal
#' @export
main_nrows_fields <- function(url) {

  x <- url |>
    request() |>
    req_url_query(
      schema  = "false",
      keys    = "false",
      results = "true",
      count   = "true",
      offset  = 0,
      size    = 1
    ) |>
    req_perform() |>
    resp_simple_json()

  list(rows   = x$meta$total_rows,
       fields = x$meta$headers,
       pages  = offset_length(x$meta$total_rows, 5000L))

}

#' Load `<CurrentMain>` API Endpoint
#' @param alias `<chr>` endpoint alias
#' @returns `<CurrentMain>` object
#' @examples
#' main_current("enrollees")
#' main_current("opt_out")
#' main_current("order_refer")
#' main_current("reassignments")
#' main_current("hospitals")
#' main_current("laboratories")
#' main_current("crosswalk")
#' main_current("rbcs")
#' main_current("facilities")
#' main_current("home_health")
#' main_current("hospice")
#' main_current("dialysis")
#' main_current("snf")
#' @autoglobal
#' @export
main_current <- function(alias) {

  x <- catalog_main()$current |>
    subset_detect(
      title,
      alias_main_current(alias)) |>
    c()

  q <- main_nrows_fields(x$identifier)

  CurrentMain(
    title       = x$title,
    description = x$description,
    contact     = x$contact,
    modified    = x$modified,
    periodicity = x$periodicity,
    temporal    = x$temporal,
    identifier  = x$identifier,
    resources   = class_resources(x$resources),
    rows        = q$rows,
    fields      = q$fields,
    pages       = q$pages,
    download    = x$download,
    dictionary  = x$dictionary,
    site        = x$site,
    references  = x$references
  )
}

#' Load `<TemporalMain>` API Endpoint
#' @param alias `<chr>` dataset title
#' @returns `<TemporalMain>` object
#' @examples
#' main_temporal("quality_payment")
#' @autoglobal
#' @export
main_temporal <- function(alias) {

  catalog_main()$temporal |>
    subset_detect(
      title,
      alias_main_temporal(alias))
}

#' Load `<TemporalMainGroup>` API Endpoints
#' @param alias `<chr>` dataset title
#' @returns `<TemporalMainGroup>` object
#' @examples
#' main_temporal_group("utilization")
#' main_temporal_group("prescribers")
#' main_temporal_group("suppliers")
#' main_temporal_group("outpatient")
#' main_temporal_group("inpatient")
#' @autoglobal
#' @export
main_temporal_group <- function(alias) {

  catalog_main()$temporal |>
    subset_detect(
      title,
      alias_main_temporal_group(alias))
}
