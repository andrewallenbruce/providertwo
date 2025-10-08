#' @autoglobal
#' @noRd
cli_sum_found <- function(found, total, pages) {
  sep <- paste0(" ", cli::col_black(cli::symbol$play), " ")
  lbl <- cli::combine_ansi_styles("silver", "italic")

  cli::cli_text(c(
    lbl("Found "),
    cli::style_bold(fmt_sum(found)),
    sep,
    lbl("Total "),
    cli::style_bold(fmt_sum(total)),
    sep,
    lbl("Pages "),
    cli::style_bold(fmt_sum(pages))
  ))
}

#' @autoglobal
#' @noRd
cli_no_match <- function(obj) {
  cli::cli_alert_warning("No {.field query} for {.field {obj@metadata@alias}}")
}

#' @autoglobal
#' @noRd
cli_yr_only <- function(obj) {
  cli::cli_alert_warning("Year-only {.field query} for {.field {obj@metadata@alias}}")
}

#' @autoglobal
#' @noRd
cli_yr_range <- function(x) {
  if (length(x) == 1L) {
    return(paste0(x, " ", theses(1L)))
  }
  rng <- range(sort.int(x), na.rm = TRUE)
  paste0(rng[1], ":", rng[2], " ", theses(length(x)))
}

#' @autoglobal
#' @noRd
cli_no_years <- function(obj) {
  cli::cli_alert_warning(
    c(
      "No {.field year} matches in {.field {obj@metadata@alias}} \n",
      "{cli::col_red(cli::symbol$pointer)} Valid years: {.pkg {cli_yr_range(obj@year)}}"
    )
  )
}

#' @autoglobal
#' @noRd
theses <- function(x) {
  paste0("(", x, ")")
}

#' @autoglobal
#' @noRd
brackets <- function(x) {
  paste0("[", x, "]")
}

#' @autoglobal
#' @noRd
just_right <- function(x) {
  format(x, justify = "right")
}

#' @autoglobal
#' @noRd
just_left <- function(x) {
  format(x, justify = "left")
}

#' @autoglobal
#' @noRd
brackets_cli <- function(x) {
  cli::cli_inform(
    paste0(
      cli::col_silver("["),
      paste0(
        cli::col_yellow(
          unlist(x, use.names = FALSE)
          ),
        collapse = cli::col_silver(", ")
        ),
    cli::col_silver("]")
    )
  )
}

#' @autoglobal
#' @noRd
brackets_cli2 <- function(x) {
  paste0(
    cli::col_black("["),
    cli::col_silver(x),
    cli::col_black("]")
  )
}

# cli_results(n = 1000, limit = 10, end = "Profile Summary", api = "Open Payments")
#' @autoglobal
#' @noRd
cli_results <- function(nres, limit, endpoint, catalog) {
  cli::cli_inform(
    c(
      "{.pkg {cli::symbol$circle_filled}} {endpoint} {.kbd {catalog}}",
      "{.pkg {cli::symbol$circle_filled}} {nres} RW{?S} {.kbd {offset(nres, limit)} PG{?S}}"
    )
  )
}

# cli_aka(end_caid$current)
# cli_aka(end_caid$temporal)
# cli_aka(end_care$current)
# cli_aka(end_care$temporal)
# cli_aka(collect_care)
# @autoglobal
# @noRd
# cli_aka <- function(aka) {
#
#   x <- unlist(aka)
#   i <- x[collapse::radixorder(names(x), sort = TRUE)]
#
#   glue_col(
#     "{bgBlack {format(names(x), justify = 'right')}}",
#     " {red {cli::symbol$pointer}} ",
#     " {silver {strtrim(unname(x), width = 60)}}",
#     x = i)
#
# }

# By default cli truncates long vectors.
# The truncation limit is by default twenty elements,
# but you can change it with the vec-trunc style.
# nms <- cli::cli_vec(names(mtcars), list("vec-trunc" = 5))
# cli::cli_text("Column names: {nms}.")

# x <- endpoint("prof_mapping")
#
# cli::boxx(
#   label        = strwrap(x@access@metadata$description,
#                          width      = 40,
#                          initial    = paste(cli::symbol$bullet, " ")),
#   header       = x@access@metadata$title,
#   footer       = "Open Payments",
#   width        = 40,
#   border_style = "round",
#   border_col   = "grey50")
#
# writeLines(c(bx, bx))

# memuse::hr(12345678, "short", digits = 2)

# fastplyr::new_tbl(
#   alias = names(aka_care$endpoint),
#   title = unlist(aka_care$endpoint, use.names = FALSE) |> glue::as_glue()
# )
#
# fuimus::print_ls(aka_care$endpoint)
#
# gsub("[[:punct:]]*", "", unlist(aka_care$endpoint, use.names = FALSE), perl = TRUE)
#
# utils::formatOL(
#   paste0(
#     format(names(aka_care$endpoint), justify = "right"),
#     " = ",
#     gsub(
#       "[[:punct:]]*", "",
#       unlist(
#         aka_care$endpoint,
#         use.names = FALSE), perl = TRUE)
#     )
#   ) |> cat(sep = "\n")
#
# cat(sprintf("%s %s %s",
#           format(names(aka_care$endpoint), justify = "right"),
#           cli::symbol$bullet,
#           gsub("[[:punct:]]*", "", unlist(aka_care$endpoint, use.names = FALSE), perl = TRUE)), sep = "\n")
#
# intToUtf8(26)
# utf8ToInt("\032")
# stringi::stri_enc_toascii(clisymbols::symbol$bullet)

#
# cli::boxx(
#   label = strwrap(x@metadata$description, width = 40, initial = paste(cli::symbol$bullet, " ")),
#   header = x@metadata$title,
#   footer = "Open Payments",
#   width = 40,
#   border_style = "round",
#   border_col = "grey")
