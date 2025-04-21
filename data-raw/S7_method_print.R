#' @include S7_care.R
#' @include S7_pro.R
#' @include S7_open.R
NULL

#' @noRd
sty <- list_tidy(
  # styles
  bdy   = cli::combine_ansi_styles(cli::style_bold, cli::col_yellow),
  bdg   = cli::combine_ansi_styles(cli::style_bold, cli::col_grey),
  ity   = cli::combine_ansi_styles(cli::style_italic, cli::col_yellow),
  itc   = cli::combine_ansi_styles(cli::style_italic, cli::col_cyan),

  # symbols
  bars  = cli::col_silver(cli::symbol$menu),
  arrow = cli::col_red(cli::symbol$pointer)
)

#' @noRd
txt <- list_tidy(
  # text
  Rows        = sty$bdy("Rows"),
  Fields      = sty$bdy("Fields"),
  Pages       = sty$bdy("Pages"),
  Modified    = sty$bdg("Modified"),
  Issued      = sty$bdg("Issued"),
  Released    = sty$bdg("Released"),
  Timespan    = sty$bdg("Timespan"),
  Periodicity = sty$bdg("Periodicity"),
  Result      = cli::col_cyan("Results"),
  Request     = cli::col_cyan("Requests")
)

#' @noRd
fmt <- list_tidy(
  # functions
  num      = \(x) prettyNum(x, big.mark = ","),
  rows     = \(x) num(x@rows),
  fields   = \(x) num(length(x@fields)),
  pages    = \(x) num(x@pages),
  temporal = \(x) sty$ity(x@temporal),
  period   = \(x) sty$ity(x@periodicity),
  modified = \(x) sty$ity(x@modified),
  issued   = \(x) sty$ity(x@issued),
  released = \(x) sty$ity(x@released),
  hyper    = \(x, nm) cli::style_hyperlink(nm, x),
  title    = \(x) cli::cat_print(cli::rule(left = x@title, width = nchar(x@title) + 6, line = 1, line_col = "silver")),
  desc     = \(x) cli::cat_line(cli::ansi_strwrap(sty$itc(x@description), width = 45, indent = 2, exdent = 2)),
  dims     = \(x) cli::cli_ul(c("{txt$Rows} {rows(x)}", "{txt$Pages} {pages(x)}", "{txt$Fields} {fields(x)}"))
)

print <- S7::new_external_generic("base", "print", "x")

S7::method(print, careMain) <- function(x) {
  fmt$title(x)
  cli::cat_line()
  fmt$desc(x)
  cli::cat_line()
  fmt$dims(x)
  cli::cat_line()

  dates <- c(
    "*" = "{txt$Modified} {sty$arrow} {fmt$modified(x)}",
    "*" = "{txt$Timespan} {sty$arrow} {fmt$temporal(x)}",
    "*" = "{txt$Periodicity} {sty$arrow} {fmt$period(x)}")

  cli::cli_bullets(dates)

  if (rlang::is_interactive()) {
    cli::cat_line()

    links <- c("i" = paste(
      fmt$hyper(x@site, "Site"),
      fmt$hyper(x@dictionary, "Dictionary"),
      fmt$hyper(x@references, "References"),
      sep = " | "))

    cli::cli_bullets(links)
    }
  invisible(x)
}

S7::method(print, proMain) <- function(x) {
  fmt$title(x)
  cli::cat_line()
  fmt$desc(x)
  cli::cat_line()
  fmt$dims(x)
  cli::cat_line()

  dates <- c(
    "*" = "{txt$Issued} {sty$arrow} {fmt$issued(x)}",
    "*" = "{txt$Modified} {sty$arrow} {fmt$modified(x)}",
    "*" = "{txt$Released} {sty$arrow} {fmt$released(x)}")

  cli::cli_bullets(dates)

  if (rlang::is_interactive()) {

    cli::cat_line()

    links <- c("i" = paste(fmt$hyper(x@site, "Site"), fmt$hyper(pro_dict(x@uuid), "Dictionary"), sep = " | "))

    cli::cli_bullets(links)
  }
  invisible(x)
}

S7::method(print, openMain) <- function(x) {
  fmt$title(x)
  cli::cat_line()
  fmt$desc(x)
  cli::cat_line()
  fmt$dims(x)
  cli::cat_line()

  cli::cli_bullets(c("*" = "{txt$Modified} {sty$arrow} {fmt$modified(x)}"))

  if (rlang::is_interactive()) {

    cli::cat_line()

    links <- c(
      "i" = paste(
        fmt$hyper("https://openpaymentsdata.cms.gov", "Site"),
        fmt$hyper("https://openpaymentsdata.cms.gov/about/glossary", "Glossary"),
        fmt$hyper("https://www.cms.gov/openpayments/downloads/openpaymentsdatadictionary.pdf", "Dictionary"),
        sep = " | "))

    cli::cli_bullets(links)
  }
  invisible(x)
}
