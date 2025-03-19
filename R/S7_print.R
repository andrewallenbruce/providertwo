#' @include S7_backend.R
NULL

print <- S7::new_external_generic("base", "print", "x")

#' @noRd
cli_dim <- function(x) {

  cli::cli_ul(
      c(
        "{CLI$sym_mn} {CLI$txt_rw} {CLI$nrows(x)}",
        "{CLI$sym_mn} {CLI$txt_pg} {CLI$npages(x)}",
        "{CLI$sym_mn} {CLI$txt_fl} {CLI$nfields(x)}",
        if (!prop_exists(x, "resources")) NULL else "{CLI$sym_mn} {CLI$txt_rs} {CLI$nfiles(x)}",
        if (!prop_exists(x, "periodicity")) NULL else "{CLI$sym_mn} {CLI$txt_per} {CLI$period(x)}"
        )
      )
}


#' @noRd
#' cli_temporal <- \(x) {
#'
#'   mod    <- cli_bold_grey("Modified")
#'   tmp    <- cli_bold_grey("Timespan")
#'   rel    <- cli_bold_grey("Released")
#'   v      <- cli::col_red(cli::symbol$pointer)
#'
#'   timespan <- cli_italic_yellow(gsub("/", " - ", prop(x, "temporal")))
#'   release  <- cli_italic_yellow(prop(x, "periodicity"))
#'   modified <- cli_italic_yellow(prop(x, "modified"))
#'
#'     cli::cli_bullets(
#'       c(
#'         "*" = "{tmp} {v} {timespan}",
#'         "*" = if (empty(prop(x, "periodicity"))) NULL else "{rel} {v} {release}",
#'         "*" = "{mod} {v} {modified}")
#'       )
#' }
#'
#' #' @noRd
#' cli_desc <- \(x) {
#'   cli::ansi_strwrap(
#'     cli_italic_cyan(x),
#'     width = 60,
#'     indent = 2,
#'     exdent = 2) |>
#'     cli::cat_line()
#' }
#'
#' #' @noRd
#' cli_title <- \(x) {
#'   cli::cat_print(
#'     cli::rule(
#'       left = cli::style_bold(x),
#'       line = 2,
#'       line_col = "silver",
#'       width = 60
#'       )
#'     )
#' }
#'
#' # Print Method for Dataset Class
#' S7::method(print, Dataset) <- function(x) {
#'   cli_title(prop(x, "title"))
#'   cli::cat_line()
#'
#'   cli_dim(x)
#'
#'   cli_desc(prop(x, "description"))
#'   cli::cat_line()
#'
#'   cli_temporal(x)
#'
#'   # cli::cli_bullets(c("*" = "Theme: {x@theme}", "*" = "Keywords: {x@keyword}", " "))
#'
#'   cli::cli_bullets(c(
#'     " ",
#'     "i" = paste0(
#'       cli::style_hyperlink("Data Dictionary", x@dictionary),
#'       " | ",
#'       cli::style_hyperlink("Landing Page", x@landingpage),
#'       " | ",
#'       cli::style_hyperlink("References", x@references)
#'     )
#'   ))
#'
#'   invisible(x)
#' }
