#' @autoglobal
#' @noRd
fmt_contactpoint <- function(x) {
  x <- delist(get_elem(x, "^has", regex = TRUE)) |>
    set_names(delist(get_elem(x, "fn")))

  as.character(glue("{names(x)} ({x})"))
}

#' @autoglobal
#' @noRd
fmt_temporal <- function(x) {
  gsub("/", paste0(" ", cli::symbol$bullet, " "), x, perl = TRUE)
}

#' @autoglobal
#' @noRd
join_on_title <- function(a, b) {
  join(
    x = a,
    y = b,
    on = "title",
    verbose = 0
  )
}

#' @autoglobal
#' @noRd
catalog_care <- function() {

  x <- fload("https://data.cms.gov/data.json", query = "/dataset")

  x <- mtt(x,
           modified    = as_date(modified),
           periodicity = fmt_periodicity(accrualPeriodicity),
           contact     = fmt_contactpoint(x$contactPoint),
           references  = delist(references),
           temporal    = fmt_temporal(temporal),
           title       = gsub("  ", " ", title, perl = TRUE),
           description = stri_trans_general(description, "latin-ascii"),
           description = gsub("[\"']", "", description, perl = TRUE),
           description = gsub("Note: This full dataset contains more records than most spreadsheet programs can handle, which will result in an incomplete load of data. Use of a database or statistical software is required.$", "", description, perl = TRUE),
           description = gsub("^ATTENTION USERS\\n\\n\\nSome Providers Opt-Out Status may end early due to COVID 19 waivers. Please contact your respective MAC for further information.\\n\\n.+\\n\\nFor more information on the opt-out process, see Manage Your Enrollment.+or view the FAQ section below.\\n\\n.+\\n\\n", "", description, perl = TRUE),
           description = gsub("\\n\\n.+\\n\\nOn November 17, 2023, CMS published in the Federal Register a final rule titled, .+Medicare and Medicaid Programs; Disclosures of Ownership and Additional Disclosable Parties Information for Skilled Nursing Facilities and Nursing Facilities; Medicare Providers.+ and Suppliers.+ Disclosure of Private Equity Companies and Real Estate Investment Trusts.+ .+88 FR 80141.+. This final rule implements parts of section 1124.+c.+ \\n\\n.+\\n\\n.+", "", description, perl = TRUE),
           description = gsub("\\n\\n.+\\n\\n", " ", description, perl = TRUE),
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

  d <- rowbind(x$distribution, fill = TRUE) |>
    fcompute(
      year       = as.integer(stri_extract_all_regex(title, "[0-9]{4}")),
      title      = stri_replace_all_regex(title, " : [0-9]{4}-[0-9]{2}-[0-9]{2}([0-9A-Za-z]{1,3})?$", ""),
      format     = cheapr_if_else(not_na(description), description, format),
      modified   = as_date(modified),
      temporal   = fmt_temporal(temporal),
      identifier = accessURL,
      download   = lag_(downloadURL, n = -1L),
      filetype   = lag_(mediaType, n = -1L),
      resources  = resourcesAPI) |>
    colorder(title) |>
    as_tbl()

  d <- sset(d, row_na_counts(d) < 4) |>
       funique(cols = c("title", "year", "format"))

  list_tidy(
    main = join_on_title(
      slt(x, -distribution),
      sbt(d, format == "latest", -format, -identifier, -modified, -temporal)) |>
      roworder(title),
    temp = join_on_title(
      sbt(d, format != "latest", -format) |>
        roworder(title, -year) |>
        f_nest_by(.cols = "title") |>
        f_ungroup(),
      slt(main, title, description, periodicity, contact, dictionary, site))
  )
}

# "https://data.cms.gov/provider-data/sites/default/files/data_dictionaries/physician/DOC_Data_Dictionary.pdf"
# pro_dict <- function(x) paste0("https://data.cms.gov/provider-data/dataset/", x, "#data-dictionary")

#' @autoglobal
#' @noRd
catalog_pro <- function() {

  x <- fload("https://data.cms.gov/provider-data/api/1/metastore/schemas/dataset/items")

  mtt(x,
      identifier  = paste0("https://data.cms.gov/provider-data/api/1/datastore/query/", identifier, "/0"),
      issued      = as_date(issued),
      modified    = as_date(modified),
      released    = as_date(released),
      group       = flatten_column(theme),
      description = stri_trim(gsub("\n", "", description, perl = TRUE)),
      download    = delist_elem(x$distribution, "downloadURL"),
      contact     = fmt_contactpoint(x$contactPoint)) |>
    slt(title, group, description, issued, modified, released, identifier, contact, download, site = landingPage) |>
    roworder(group, title) |>
    as_tbl()
}

#' @autoglobal
#' @noRd
catalog_open <- function() {

  x <- fload("https://openpaymentsdata.cms.gov/api/1/metastore/schemas/dataset/items?show-reference-ids")

  x <- mtt(x,
           identifier  = paste0("https://openpaymentsdata.cms.gov/api/1/datastore/query/", identifier, "/0"),
           modified    = as_date(modified),
           year        = get_data_elem(keyword),
           year        = stri_replace_all_fixed(year, c("all years"), c("All")),
           year        = cheapr_if_else(title == "Provider profile ID mapping table", "All", year),
           title       = toTitleCase(title),
           contact     = fmt_contactpoint(x$contactPoint),
           description = gsub("[\"']", "", description, perl = TRUE),
           description = gsub("\r\n", " ", description, perl = TRUE),
           description = gsub("<p><strong>NOTE: </strong>This is a very large file and, depending on your network characteristics and software, may take a long time to download or fail to download. Additionally, the number of rows in the file may be larger than the maximum rows your version of <a href=https://support.microsoft.com/en-us/office/excel-specifications-and-limits-1672b34d-7043-467e-8e27-269d656771c3>Microsoft Excel</a> supports. If you cant download the file, we recommend engaging your IT support staff. If you are able to download the file but are unable to open it in MS Excel or get a message that the data has been truncated, we recommend trying alternative programs such as MS Access, Universal Viewer, Editpad or any other software your organization has available for large datasets.</p>$", "", description, perl = TRUE),
           description = gsub("  ", " ", description, perl = TRUE),
           description = stri_trim(description),
           download    = delist(get_elem(get_elem(x$distribution, "data", DF.as.list = TRUE), "downloadURL"))) |>
    slt(year, title, description, modified, identifier, contact, download) |>
    as_tbl()

  list(main = roworder(sbt(x, year == "All", -year), title),
       temp = roworder(sbt(x, year != "All") |> mtt(year = as.integer(year), title = stri_replace_all_regex(title, "^[0-9]{4} ", "")), title, -year)
  )
}

#' @autoglobal
#' @noRd
catalogs <- function() {
  list(
    care = catalog_care(),
    pro  = catalog_pro(),
    open = catalog_open(),
    caid = catalog_caid()
  )
}

rlang::on_load(.catalog <<- catalogs())
