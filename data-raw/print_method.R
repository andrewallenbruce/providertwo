x <- public_Dataset("laboratories")

x

props(x)

knit_print <- S7::new_external_generic("knitr", "knit_print", "x")


# Print Method for Dataset Class
S7::method(knit_print, Dataset) <- function(x) {

  prop(prop(x, "resources"), "files") |>
    gt::gt(rowname_col = "name") |>
    gt::tab_header(title = gt::md("**Resource File Downloads**")) |>
    gt::cols_hide(columns = "fileType") |>
    gt::fmt_url(columns = downloadURL,
                label = \(x) toupper(tools::file_ext(x)),
                as_button = TRUE,
                button_fill = "white",
                show_underline = TRUE,
                button_width = gt::px(40),
                button_outline = "black") |>
    gt::cols_merge(columns = c(downloadURL, fileSize), pattern = "<b>{1}</b> ({2})") |>
    gt::opt_table_font(font = gt::google_font("Roboto Mono")) |>
    gt::opt_vertical_padding(scale = 0.4) |>
    gt::opt_horizontal_padding(scale = 2) |>
    gt::opt_align_table_header(align = "left") |>
    gt::opt_table_outline() |>
    gt::tab_options(column_labels.hidden = TRUE,
                    table.font.size = 12)


  props(x)



  prop(prop(x, "identifier"),"rows")
  length(prop(prop(x, "identifier"), "fields"))
  offset_length(prop(prop(x, "identifier"), "rows"), limit = 5000)
  nrow(prop(prop(x, "resources"), "files"))



  cli::cli_bullets(
    # c("{cli::symbol$menu}" = paste0(
    #   cli::style_bold('Rows'),
    #   ": {prettyNum(x@identifier@rows, big.mark = ',')} | ",
    #   cli::style_bold("Fields"),
    #   ": {length(x@identifier@fields)} | ",
    #   cli::style_bold("Resources"),
    #   ": {nrow(x@resources@files)} files"),
    #   " ",
    c("*" = "Periodicity: {x@periodicity}",
      "*" = "Last Modified: {x@modified}",
      "*" = "Time Period: {gsub('/', ' - ', x@temporal)}",
      "*" = "Theme: {x@theme}",
      "*" = "Keywords: {x@keyword}",
      " "))

  cli::cli_bullets(
    c(" ",
      "i" = paste0(
        cli::style_hyperlink("Data Dictionary", x@dictionary), " | ",
        cli::style_hyperlink("Landing Page", x@landingpage), " | ",
        cli::style_hyperlink("References", x@references))))

  invisible(x)
}
