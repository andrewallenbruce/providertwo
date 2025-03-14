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
      contact     = reduce_contact(x$contactPoint),
      references  = delist(references),
      title       = gsub("  ", " ", title, perl = TRUE),
      description = gsub("[\"']", "", description, perl = TRUE),
      description = gsub("Note: This full dataset contains more records than most spreadsheet programs can handle, which will result in an incomplete load of data. Use of a database or statistical software is required.$", "", description, perl = TRUE),
      description = gsub("^ATTENTION USERSSome Providers Opt-Out Status may end early due to COVID 19 waivers. Please contact your respective MAC for further information. For more information on the opt-out process, see Manage Your Enrollment or view the FAQ section below. $", "", description, perl = TRUE),
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

  d <- get_elem(x, "distribution") |>
    rowbind(fill = TRUE) |>
    as_tbl() |>
    fcompute(
      year         = as_int(stri_extract_all_regex(title, "[0-9]{4}")),
      title        = stri_replace_all_regex(title, " : [0-9]{4}-[0-9]{2}-[0-9]{2}([0-9A-Za-z]{1,3})?$", ""),
      format       = cheapr_if_else(not_na(description), description, format),
      modified     = as_date(modified),
      temporal     = temporal,
      identifier   = accessURL,
      download     = lag_(downloadURL, n = -1L),
      filetype     = lag_(mediaType, n = -1L),
      resources    = resourcesAPI) |>
    colorder(title)

  d <- sset(d, row_na_counts(d) < 4) |>
    funique(cols = c("title", "year", "format")) |>
    fcount(title, add = TRUE)

  list_tidy(
    current = roworder(join(slt(x, -distribution), sbt(d, format == "latest", -format, -identifier, -modified, -temporal), on = "title", verbose = 0), title),
    temporal = join(roworder(d, title, -year) |> f_nest_by(.cols = "title") |> f_ungroup(),
                    slt(current, title, description, periodicity, contact, dictionary, site),
                    on = "title",
                    verbose = 0))
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
