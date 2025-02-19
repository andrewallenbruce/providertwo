#' @include classes.R
NULL

print  <- S7::new_external_generic("base", "print", "x")
format <- S7::new_external_generic("base", "format", "x")


cli_dim <- \(x) {
  s      <- cli::col_silver(cli::symbol$menu)
  rows   <- cli::style_bold(cli::col_yellow("Rows"))
  fields <- cli::style_bold(cli::col_yellow("Fields"))
  pages  <- cli::style_bold(cli::col_yellow("Pages"))
  files  <- cli::style_bold(cli::col_yellow("Resources"))

  n <- prettyNum(prop(prop(x, "identifier"), "rows"), ',')
  f <- length(prop(prop(x, "identifier"), "fields"))
  o <- offset_length(prop(prop(x, "identifier"), "rows"), limit = 5000)
  r <- nrow(prop(prop(x, "resources"), "files"))

  cli::cat_line(cli::cli_text(
    '{s} {rows} {n} {s} {fields} {f} {s} {pages} {o} {s} {files} {r} {s}'
  ))
}

cli_temporal <- \(x) {
  v   <- cli::col_red(cli::symbol$pointer)
  mod <- cli::style_bold(cli::col_grey("Modified"))
  tmp <- cli::style_bold(cli::col_grey("Timespan"))
  rel <- cli::style_bold(cli::col_grey("Released"))

  timespan <- cli::style_italic(cli::col_yellow(gsub("/", " - ", prop(x, "temporal"))))
  release  <- cli::style_italic(cli::col_yellow(prop(x, "periodicity")))
  modified <- cli::style_italic(cli::col_yellow(prop(x, "modified")))

  cli::cat_line(cli::cli_text('{tmp} {v} {timespan} {rel} {v} {release} {mod} {v} {modified}'))
}

cli_desc <- \(x) {
  cli::ansi_strwrap(
    cli::col_cyan(cli::style_italic(x)),
    width = 60,
    indent = 2,
    exdent = 2
  ) |>
    cli::cat_line()
}

cli_title <- \(x) {
  cli::cat_print(cli::rule(
    left = cli::style_bold(x),
    line = 2,
    line_col = "silver",
    width = 60
  ))
}

cli_org <- \(x) {
  cli::cat_print(cli::rule(
    left = cli::style_bold(x),
    line = 2,
    line_col = "silver",
    width = 60
  ))
}


# Print Method for Dataset Class
S7::method(print, Dataset) <- function(x) {
  cli_title(prop(x, "title"))
  cli::cat_line()

  cli_dim(x)

  cli_desc(prop(x, "description"))
  cli::cat_line()

  cli_temporal(x)

  # cli::cli_bullets(c("*" = "Theme: {x@theme}", "*" = "Keywords: {x@keyword}", " "))

  cli::cli_bullets(c(
    " ",
    "i" = paste0(
      cli::style_hyperlink("Data Dictionary", x@dictionary),
      " | ",
      cli::style_hyperlink("Landing Page", x@landingpage),
      " | ",
      cli::style_hyperlink("References", x@references)
    )
  ))

  invisible(x)
}

# S7::method(print, Dataset) <- function(x) {
#
#   cli::cli_h3(
#     paste0(
#       cli::col_red("{.emph Dataset} "),
#       cli::style_bold(gsub("  ", " ", x@title)))
#     )
#
#   cli::cli_bullets(
#     c(">" = paste0(
#       cli::style_bold("Rows"),
#       ": {prettyNum(x@identifier@rows, big.mark = ',')} | ",
#       cli::style_bold("Fields"),
#       ": {length(x@identifier@fields)} | ",
#       cli::style_bold("Resources"),
#       ": {nrow(x@resources@files)} files"),
#       " ",
#       "*" = "Periodicity: {x@periodicity}",
#       "*" = "Last Modified: {x@modified}",
#       "*" = "Time Period: {gsub('/', ' - ', x@temporal)}",
#       "*" = "Theme: {x@theme}",
#       "*" = "Keywords: {x@keyword}",
#       " "))
#
#   cli::cli_text(
#     cli::style_italic(
#     if (sf_chars(x@description) <= 400)
#       x@description else
#         paste0(sf_sub(x@description,
#                       start = 1,
#                       stop = 400),
#                "...[truncated]")))
#
#   cli::cli_bullets(
#     c(" ",
#       "i" = paste0(
#       cli::style_hyperlink("Data Dictionary", x@dictionary), " | ",
#       cli::style_hyperlink("Landing Page", x@landingpage), " | ",
#       cli::style_hyperlink("References", x@references))))
#
#   invisible(x)
# }
