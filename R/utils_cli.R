CLI <- list_tidy(

    # styles
    bold_yell = cli::combine_ansi_styles(cli::style_bold, cli::col_yellow),
    bold_grey = cli::combine_ansi_styles(cli::style_bold, cli::col_grey),
    ital_yell = cli::combine_ansi_styles(cli::style_italic, cli::col_yellow),
    ital_cyan = cli::combine_ansi_styles(cli::style_italic, cli::col_cyan),

    # symbols
    sym_mn = cli::col_silver(cli::symbol$menu),
    sym_pt = cli::col_red(cli::symbol$pointer),

    # text
    txt_rw   = bold_yell("Rows"),
    txt_fl   = bold_yell("Fields"),
    txt_pg   = bold_yell("Pages"),
    txt_rs   = bold_yell("Resources"),
    txt_mod  = bold_grey("Modified"),
    txt_span = bold_grey("Timespan"),
    txt_per  = bold_grey("Periodcity"),
    txt_res  = cli::col_cyan("Result(s)"),
    txt_req  = cli::col_cyan("Request(s)"),

    # functions
    num     = function(x) prettyNum(x, big.mark = ","),
    title   = function(x) cli::cat_print(cli::rule(left = cli::style_bold(x@title), width = 50, line = 2, line_col = "silver")),
    desc    = function(x) cli::cat_line(cli::ansi_strwrap(ital_cyan(x@description), width = 50, indent = 2, exdent = 2)),
    nrows   = function(x) num(x@rows),
    nfields = function(x) num(length(x@fields)),
    npages  = function(x) num(x@pages),
    nfiles  = function(x) unlisted_length(x@resources@files$file),
    temp    = function(x) ital_yellow(x@temporal),
    period  = function(x) ital_yellow(x@periodicity),
    mod     = function(x) ital_yellow(x@modified),
    link    = function(x, nm) cli::style_hyperlink(nm, x)
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

  # pg <- CLI$bold_yell(CLI$num(offset_length(n, limit)))
  # n  <- CLI$bold_yell(CLI$num(n))

  cli::cli_inform(
    c(
      " " = "{CLI$bold_yell(CLI$num(n))} {CLI$txt_res} {CLI$sym_mn} {CLI$bold_yell(CLI$num(offset_length(n, limit)))} {CLI$txt_req}",
      " " = " "
      )
    )
}
