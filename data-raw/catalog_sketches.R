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
