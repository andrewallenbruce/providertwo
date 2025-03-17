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
  tt = \(x) cli::cat_print(cli::rule(left = cli::style_bold(x@title), line = 2, line_col = "silver", width = 60)),
  ds = \(x) cli::cat_line(cli::ansi_strwrap(cli_pt$italic_cyan(x@description), width = 60, indent = 2, exdent = 2)),
  no = \(x) prettyNum(x@rows, ','),
  nf = \(x) length(x@fields),
  nr = \(x) cheapr::unlisted_length(x@resources@files$file),
  tm = \(x) cli_pt$italic_yellow(x@temporal),
  pr = \(x) cli_pt$italic_yellow(x@periodicity),
  md = \(x) cli_pt$italic_yellow(x@modified),
  lk = \(prop, nm) cli::style_hyperlink(nm, prop)
)


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
