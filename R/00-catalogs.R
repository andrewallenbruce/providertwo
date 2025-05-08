#' @autoglobal
#' @noRd
catalog_care <- function() {

  x <- fload("https://data.cms.gov/data.json", query = "/dataset")

  x <- mtt(
    x,
    modified    = as_date(modified),
    periodicity = fmt_periodicity(accrualPeriodicity),
    contact     = fmt_contactpoint(x$contactPoint),
    references  = delist(references),
    temporal    = fmt_temporal(temporal),
    title       = greplace(title, "  ", " "),
    title       = remove_non_ascii(title),
    description = stri_trans_general(description, "latin-ascii"),
    description = remove_non_ascii(description),
    description = greplace(description, "[\"']", ""),
    description = greplace(description, "Note: This full dataset contains more records than most spreadsheet programs can handle, which will result in an incomplete load of data. Use of a database or statistical software is required.$", ""),
    description = greplace(description, "^ATTENTION USERSSome Providers Opt-Out Status may end early due to COVID 19 waivers. Please contact your respective MAC for further information. For more information on the opt-out process, see Manage Your Enrollment or view the FAQ section below. ", ""),
    description = greplace(description, "On November 17, 2023, CMS published in the Federal Register a final rule titled, .+Medicare and Medicaid Programs; Disclosures of Ownership and Additional Disclosable Parties Information for Skilled Nursing Facilities and Nursing Facilities; Medicare Providers.+ and Suppliers.+ Disclosure of Private Equity Companies and Real Estate Investment Trusts.+ .+88 FR 80141.+. This final rule implements parts of section 1124.+c.+ \\n\\n.+\\n\\n.+", ""),
    description = greplace(description, "\\n\\n.+\\n\\n", ""),
    description = stri_trim(description)
  ) |>
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
      distribution
    ) |>
    as_tbl()

  d <- rowbind(x$distribution, fill = TRUE) |>
    fcompute(
      year       = extract_year(title),
      title      = stri_replace_all_regex(title, " : [0-9]{4}-[0-9]{2}-[0-9]{2}([0-9A-Za-z]{1,3})?$", ""),
      format     = cheapr_if_else(!is_na(description), description, format),
      modified   = as_date(modified),
      temporal   = fmt_temporal(temporal),
      identifier = accessURL,
      download   = lag_(downloadURL, n = -1L),
      filetype   = lag_(mediaType, n = -1L),
      resources  = resourcesAPI
    ) |>
    colorder(title) |>
    as_tbl()

  d <- sset(d, row_na_counts(d) < 4) |> funique(cols = c("title", "year", "format"))

  list_tidy(
    main = join_on_title(
      slt(x, -distribution),
      sbt(d, format == "latest", title, download, resources)) |> roworder(title),
    temp = join_on_title(
      sbt(d, format != "latest" & title %!in_% care_types("single"), -format, -filetype) |>
        roworder(title, -year) |>
        f_nest_by(.by = title) |>
        f_ungroup(),
      slt(main, title, description, periodicity, contact, dictionary, site)
    )
  )
}

# "https://data.cms.gov/provider-data/sites/default/files/data_dictionaries/physician/DOC_Data_Dictionary.pdf"
#' @autoglobal
#' @noRd
catalog_pro <- function() {
  x <- fload("https://data.cms.gov/provider-data/api/1/metastore/schemas/dataset/items")

  x <- mtt(x,
    title       = remove_non_ascii(title),
    dictionary  = paste0("https://data.cms.gov/provider-data/dataset/", identifier, "#data-dictionary"),
    identifier  = paste0("https://data.cms.gov/provider-data/api/1/datastore/query/", identifier, "/0"),
    issued      = as_date(issued),
    modified    = as_date(modified),
    released    = as_date(released),
    group       = flatten_column(theme),
    description = stri_trim(greplace(description, "\n", "")),
    download    = delist_elem(x$distribution, "downloadURL"),
    contact     = fmt_contactpoint(x$contactPoint)) |>
    slt(title, group, description, issued, modified, released, identifier, contact, download, site = landingPage, dictionary) |>
    roworder(group, title) |>
    as_tbl()

  list(
   end  = sbt(x, group != "Physician office visit costs"),
   cost = sbt(x, group == "Physician office visit costs")
  )
}

#' @autoglobal
#' @noRd
catalog_open <- function() {

  x <- fload("https://openpaymentsdata.cms.gov/api/1/metastore/schemas/dataset/items?show-reference-ids")

  x <- mtt(x,
    identifier  = paste0("https://openpaymentsdata.cms.gov/api/1/datastore/query/", identifier, "/0"),
    modified    = as_date(modified),
    year        = get_data_elem(keyword),
    year        = greplace(year, "all years", "All"),
    year        = cheapr_if_else(title == "Provider profile ID mapping table", "All", year),
    title       = remove_non_ascii(title),
    title       = toTitleCase(title),
    contact     = fmt_contactpoint(x$contactPoint),
    description = greplace(description, "[\"']", ""),
    description = greplace(description, "\r\n", " "),
    description = greplace(description, "<p><strong>NOTE: </strong>This is a very large file and, depending on your network characteristics and software, may take a long time to download or fail to download. Additionally, the number of rows in the file may be larger than the maximum rows your version of <a href=https://support.microsoft.com/en-us/office/excel-specifications-and-limits-1672b34d-7043-467e-8e27-269d656771c3>Microsoft Excel</a> supports. If you cant download the file, we recommend engaging your IT support staff. If you are able to download the file but are unable to open it in MS Excel or get a message that the data has been truncated, we recommend trying alternative programs such as MS Access, Universal Viewer, Editpad or any other software your organization has available for large datasets.</p>$", ""),
    description = greplace(description, "  ", " "),
    description = stri_trim(description),
    download    = get_elem(x$distribution, "data", DF.as.list = TRUE) |> get_elem("downloadURL") |> delist()) |>
    slt(year,
        title,
        description,
        modified,
        identifier,
        contact,
        download) |>
    as_tbl()

  list(
    main = sbt(x, year == "All", -year) |> roworder(title),
    temp = sbt(x, year != "All") |>
      mtt(year        = as.integer(year),
          title       = stri_replace_all_regex(title, "^[0-9]{4} ", ""),
          description = case(title == "General Payment Data"   ~ "All general (non-research, non-ownership related) payments from the program year",
                             title == "Ownership Payment Data" ~ "All ownership and investment payments from the program year",
                             title == "Research Payment Data"  ~ "All research-related payments from the program year",
                             .default = description)) |>
      roworder(title, -year) |>
      f_nest_by(.by = c(title, description, modified)) |>
      f_ungroup()
    )
}

#' @name catalogs
#' @title API Catalogs
#' @description
#' List of API catalogs:
#'   * `catalog_care`: CMS Medicare API
#'   * `catalog_pro`: CMS Provider API
#'   * `catalog_open`: CMS Open Payments API
#'   * `catalog_caid`: CMS Medicaid API
#'   * `catalog_hgov`: CMS HealthCare.gov API
#' @returns `<list>` of catalogs
#' @examples
#' catalogs()
#' @autoglobal
#' @export
catalogs <- function() {
  list(
    care = catalog_care(),
    pro  = catalog_pro(),
    open = catalog_open(),
    caid = catalog_caid(),
    hgov = catalog_hgov()
  )
}

rlang::on_load(.catalog <<- catalogs())
