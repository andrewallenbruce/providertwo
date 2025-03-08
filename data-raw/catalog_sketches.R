.onLoad <- function(libname, pkgname) {

  # if (httr2::is_online()) {
  #   .api__public   <<- load_public()
  #   .api__provider <<- load_provider()
  #   .api__openpay  <<- load_openpayments()
  # }

  catalog_provider <<- memoise::memoise(catalog_provider)
  catalog_open     <<- memoise::memoise(catalog_open)
  catalog_main     <<- memoise::memoise(catalog_main)
  open_dictionary  <<- memoise::memoise(open_dictionary)

  S7::methods_register()
}

# .onUnload <- function(libpath) {
#   remove(
#     list = c(
#       ".api__public",
#       ".api__provider",
#       ".api__openpay"
#       ),
#     envir = .GlobalEnv)
# }

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

replace_open_columns  <- \(x) replace_fixed(x, c(":", "%", "@", "$", "properties_"), c("_", "", "", "", "pr_"))
replace_open_desc     <- \(x) replace_fixed(x, c("\n", "<p><strong>NOTE: </strong>This is a very large file and, depending on your network characteristics and software, may take a long time to download or fail to download. Additionally, the number of rows in the file may be larger than the maximum rows your version of <a href=\"https://support.microsoft.com/en-us/office/excel-specifications-and-limits-1672b34d-7043-467e-8e27-269d656771c3\">Microsoft Excel</a> supports. If you can't download the file, we recommend engaging your IT support staff. If you are able to download the file but are unable to open it in MS Excel or get a message that the data has been truncated, we recommend trying alternative programs such as MS Access, Universal Viewer, Editpad or any other software your organization has available for large datasets.</p>"), c(". ", ""))

#' Wrapper for `terse::terse()`
#' @param x `<list>` or `<data.frame>` to be printed
#' @param p `<chr>` prefix to be used for each line
#' @param w `<int>` target width; 0 = auto; -1 = no limit
#' @param m `<int>` maximum vector length anywhere in original object
#' @param s `<chr>` separator to be used for each line
#' @param a `<chr>` Use ANSI to colour output? default: FALSE
#' @returns `<chr>` terse representation of `x`
#' @autoglobal
#' @keywords internal
#' @export
glimst <- \(x,
            p = "- ",
            w = 0,
            m = 20,
            s = " ",
            a = FALSE) {

  terse::terse(
    x           = x,
    prefix      = p,
    width       = w,
    max_vec_len = m,
    config      = list(
      gsep      = s,
      ansi      = a)
  )
}

api <- as_Dataset("Public Provider Enrollment")

S7::method(print, Dataset) <- function(x, ...) {

  ob <- props(x)
  ob$identifier <- NULL
  ob$resourcesAPI <- NULL
  ob$description <- substr(ob@description, 1, 415)
  id <- props(x@identifier)
  re <- props(x@resourcesAPI)
  # re_files <- as.list(re$files)
  # re$files <- NULL

  ob

  glue::glue_data(
    x@resourcesAPI@files,
    "{.strong {.field <<name>>}} >=> <<fileSize>>",
    .open = "<<",
    .close = ">>")


  paste(
    glue::glue(
      '"*" = cli::style_hyperlink("{text}", {url})',
      text = c("Landing Page", "Data Dictionary"),
      url = c("x@landingPage", "x@describedBy")
    )) |>
    rlang::parse_expr() |>
    rlang::eval_tidy()

  cat(
    c(cli::cli_h2(c(cli::col_red("API: "), cli::col_blue(x@title))),
      cli::cli_par(cli::col_silver(x@description))),
    cli::cli_h3(cli::col_yellow("Hyperlinks")),
    cli::cli_bullets(
      c("*" = cli::style_hyperlink("Landing Page", x@landingPage),
        "*" = cli::style_hyperlink("Data Dictionary", x@describedBy)))
  )

  cat(
    c(cli::cli_h2(c(cli::col_red("API: "), cli::col_blue(x@title)))), "\n",
    cli::cli_par(cli::col_silver(x@description)), "\n\n",
    "Accrual Periodicity: ", x@accrualPeriodicity, "\n",
    "Last Modified: ", format(x@modified), "\n",
    "Date Range: ", x@temporal, "\n",
    "Total Rows: ", format(x@identifier@rows), "\n"
  )
}
