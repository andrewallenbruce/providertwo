#' Print a named list
#'
#' @param ls `<list>` to print
#'
#' @param prefix `<chr>` to prepend to each line
#'
#' @returns `<list>` invisibly
#'
#' @examples
#' print_list(list(a = 1, b = 2, c = 3))
#'
#' @autoglobal
#'
#' @export
print_list <- function(ls, prefix = "") {

  if (length(ls) == 0) cat("<empty>\n")

  ns <- names(ls)

  if (length(ns) != length(ls)) stop("all elements must be named")

  ls <- lapply(ls, as.character)

  cat(sprintf("%s%s : %s", prefix, format(ns), ls), sep = "\n")

  invisible(ls)
}

#' #' @include classes.R
#' NULL
#'
#' print  <- S7::new_external_generic("base", "print", "x")
#' format <- S7::new_external_generic("base", "format", "x")
#'
#'
#' cli_bold_yellow   <- cli::combine_ansi_styles(cli::style_bold, cli::col_yellow)
#' cli_italic_yellow <- cli::combine_ansi_styles(cli::style_italic, cli::col_yellow)
#' cli_bold_grey     <- cli::combine_ansi_styles(cli::style_bold, cli::col_grey)
#' cli_italic_cyan   <- cli::combine_ansi_styles(cli::style_italic, cli::col_cyan)
#'
#'
#'
#' #' @noRd
#' cli_dim <- \(x) {
#'   rows   <- cli_bold_yellow("Rows")
#'   fields <- cli_bold_yellow("Fields")
#'   pages  <- cli_bold_yellow("Pages")
#'   files  <- cli_bold_yellow("Resources")
#'
#'   s <- cli::col_silver(cli::symbol$menu)
#'   n <- prettyNum(prop(prop(x, "identifier"), "rows"), ',')
#'   f <- length(prop(prop(x, "identifier"), "fields"))
#'   o <- offset_length(prop(prop(x, "identifier"), "rows"), limit = 5000)
#'   r <- nrow(prop(prop(x, "resources"), "files"))
#'
#'   cli::cat_line(
#'     cli::cli_text(
#'       c(
#'         "{s} {rows} {n} ",
#'         "{s} {fields} {f} ",
#'         "{s} {pages} {o} ",
#'         if (empty(prop(x, "periodicity"))) "{s} {files} 0 " else "{s} {files} {r} ",
#'         "{s}")
#'       )
#'     )
#' }
#'
#' #' @noRd
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
