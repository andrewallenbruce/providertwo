#' Format Number with Commas
#' @param x `<int>` number to format
#' @returns `<chr>` formatted number
#' @autoglobal
#' @keywords internal
#' @export
num <- function(x) prettyNum(x, big.mark = ",")

#' @noRd
cli_pt <- list_tidy(
  bold_yellow   = cli::combine_ansi_styles(cli::style_bold, cli::col_yellow),
  bold_grey     = cli::combine_ansi_styles(cli::style_bold, cli::col_grey),
  italic_yellow = cli::combine_ansi_styles(cli::style_italic, cli::col_yellow),
  italic_cyan   = cli::combine_ansi_styles(cli::style_italic, cli::col_cyan),

  sym_menu      = cli::col_silver(cli::symbol$menu),
  sym_point     = cli::col_red(cli::symbol$pointer),

  txt_row       = bold_yellow("Rows"),
  txt_field     = bold_yellow("Fields"),
  txt_page      = bold_yellow("Pages"),
  txt_file      = bold_yellow("Resources"),
  txt_mod       = bold_grey("Modified"),
  txt_span      = bold_grey("Timespan"),
  txt_period    = bold_grey("Periodcity")
)

#' @noRd
cli_fn <- list(
  title = \(x) cli::cat_print(cli::rule(left = cli::style_bold(x@title), line = 2, line_col = "silver", width = 60)),
  desc = \(x) cli::cat_line(cli::ansi_strwrap(cli_pt$italic_cyan(x@description), width = 60, indent = 2, exdent = 2)),
  nrows = \(x) num(x@rows),
  nfields = \(x) length(x@fields),
  nfiles = \(x) unlisted_length(x@resources@files$file),
  temp = \(x) cli_pt$italic_yellow(x@temporal),
  period = \(x) cli_pt$italic_yellow(x@periodicity),
  mod = \(x) cli_pt$italic_yellow(x@modified),
  link = \(prop, nm) cli::style_hyperlink(nm, prop)
)

#' Inform number of results and requests
#'
#' @param n     `<int>` Number of results returned in an API request
#'
#' @param limit `<int>` API rate limit, i.e. the maximum number of results an
#'                      API will return per request.
#' @returns cli message
#' @autoglobal
#' @keywords internal
#' @export
cli_results <- function(n, limit) {

  pg  <- offset_length(n, limit)
  res <- ifelse(n > 1, cli::col_cyan("Results"), cli::col_cyan("Result"))
  req <- ifelse(pg > 1, cli::col_cyan("Requests"), cli::col_cyan("Request"))

  pg <- cli_pt$bold_yellow(num(pg))
  n  <- cli_pt$bold_yellow(num(n))

  cli::cli_inform(
    c(" " = " ",
      "i" = "{n} {res} {cli_pt$sym_menu} {pg} {req}",
      " " = " "))
}


# x <- CurrentMain("enrollees")
# cli_pt$sym_menu
# cli_pt$sym_point
#
# cli_fn$tt(x)
# cli_fn$ds(x)
#
# cli_fn$no(x)
# cli_fn$nf(x)
# cli_fn$nr(x)
# cli_fn$tm(x)
