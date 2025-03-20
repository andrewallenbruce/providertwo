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
      temporal    = main_temp(temporal),
      title       = gsub("  ", " ", title, perl = TRUE),
      description = gsub("[\"']", "", description, perl = TRUE),
      description = gsub("Note: This full dataset contains more records than most spreadsheet programs can handle, which will result in an incomplete load of data. Use of a database or statistical software is required.$", "", description, perl = TRUE),
      description = gsub("^ATTENTION USERS\\n\\n\\nSome Providers Opt-Out Status may end early due to COVID 19 waivers. Please contact your respective MAC for further information.\\n\\n.+\\n\\nFor more information on the opt-out process, see Manage Your Enrollment.+or view the FAQ section below.\\n\\n.+\\n\\n", "", description, perl = TRUE),
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
      year       = as_int(stri_extract_all_regex(title, "[0-9]{4}")),
      title      = stri_replace_all_regex(title, " : [0-9]{4}-[0-9]{2}-[0-9]{2}([0-9A-Za-z]{1,3})?$", ""),
      format     = cheapr_if_else(not_na(description), description, format),
      modified   = as_date(modified),
      temporal   = main_temp(temporal),
      identifier = accessURL,
      download   = lag_(downloadURL, n = -1L),
      filetype   = lag_(mediaType, n = -1L),
      resources  = resourcesAPI) |>
    colorder(title)

  d <- sset(d, row_na_counts(d) < 4) |>
    funique(cols = c("title", "year", "format"))
    # fcount(title, add = TRUE)

  list_tidy(
    current = roworder(join(
      slt(x, -distribution),
      sbt(d, format == "latest", -format, -identifier, -modified, -temporal),
      on = "title",
      verbose = 0
    ), title),
    temporal = join(
      roworder(
        sbt(d, format != "latest", -format),
        title, -year) |>
        f_nest_by(.cols = "title") |>
        f_ungroup(),
      slt(
        current,
        title,
        description,
        periodicity,
        contact,
        dictionary,
        site
      ),
      on = "title",
      verbose = 0
    )
  )
}

#' Get Field Names and Number of Rows from Main Endpoint
#' @param url `<chr>` endpoint URL
#' @returns `<list>` number of rows and field names
#' @autoglobal
#' @keywords internal
#' @export
main_dims <- function(url) {

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
    perform_simple()

  list(rows   = x$meta$total_rows,
       fields = x$meta$headers,
       pages  = offset_length(x$meta$total_rows, 5000L))

}

#' Get Field Names and Number of Rows from Main Endpoint
#' @param url `<chr>` endpoint URL
#' @returns `<list>` number of rows and field names
#' @autoglobal
#' @keywords internal
#' @export
main_temporal_dims <- function(url) {
  list_tidy(
    rows   = perform_simple(req_url_path_append(request(url), "stats")) |> _[["total_rows"]],
    pages  = offset_length(rows, 5000L),
    fields = names(perform_simple(request(url)))
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

  x <- subset_detect(
    catalog_main()$temporal,
    title,
    alias_main_temporal_group(alias)) |>
    mtt(
      set   = stri_extract_first_regex(title, "(?<=-\\s).*$"),
      title = stri_extract_first_regex(title, "^.*(?=\\s-)"))

  clean_names <- \(x) gsub(" ", "_", tolower(x), perl = TRUE)
  set_clean   <- \(i, x) set_names(i, clean_names(x))

  list_tidy(
    title       = x$title[1],
    periodicity = x$periodicity[1],
    contact     = x$contact[1],
    description = set_clean(x$description, x$set),
    dictionary  = set_clean(x$dictionary, x$set),
    site        = set_clean(x$site, x$set),
    !!!set_clean(as.list(x$data), x$set)
  )
}
