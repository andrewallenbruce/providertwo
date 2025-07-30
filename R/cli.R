# x <- new_query(
#   first_name = starts_with("Andr"),
#   last_name  = contains("J"),
#   state      = any_of(c("CA", "GA", "NY")),
#   city       = equals(c("Atlanta", "Los Angeles"), negate = TRUE),
#   state_own  = c("GA", "MD"),
#   npi        = npi_ex$k,
#   ccn        = "01256",
#   pac        = NULL,
#   rate       = between(0.45, 0.67),
#   year       = 2014:2025)
#
#   cli_query(x)
#' @autoglobal
#' @noRd
cli_query <- function(x) {

  x <- x@input

  if (any_evaled(x)) x[are_evaled(x)] <- eval_cli(x[are_evaled(x)])
  if (any_mods(x))   x[are_mods(x)]   <- mods_cli(x[are_mods(x)])
  if (any_calls(x))  x[are_calls(x)]  <- call_cli(x[are_calls(x)])

  FIELD  <- just_right(names(x))
  EQUALS <- cli::col_black(cli::style_bold(cli::symbol$double_line))
  VALUE  <- just_left(unlist(x, use.names = FALSE))

  cli::cli_h1("New Query:")
  glue::glue_safe("{FIELD} {EQUALS} {VALUE}")

  invisible(x)
}

# cli_results(n = 1000, limit = 10, end = "Profile Summary", api = "Open Payments")
#' @autoglobal
#' @noRd
cli_results <- function(n, limit, end, api) {
  cli::cli_inform(
    c(
      "{.pkg {cli::symbol$circle_filled}} {end} {.kbd {api}}",
      "{.pkg {cli::symbol$circle_filled}} {n} RW{?S} {.kbd {offset_size(n, limit)} PG{?S}}"
    )
  )
}

# cli_aka(end_caid$current)
# cli_aka(end_caid$temporal)
# cli_aka(end_care$current)
# cli_aka(end_care$temporal)
# cli_aka(collect_care)
#' @autoglobal
#' @noRd
cli_aka <- function(aka) {

  x <- unlist(aka)
  i <- x[radixorder(names(x), sort = TRUE)]

  glue_col(
    "{bgBlack {format(names(x), justify = 'right')}}",
    # " {red {cli::symbol$pointer}} ",
    " {silver {strtrim(unname(x), width = 60)}}",
    x = i)

}

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

#   list(
#     caid = aka_caid$endpoint,
#     care = aka_care$endpoint,
#     prov = aka_prov$endpoint,
#     open = aka_open$endpoint,
#     hgov = aka_hgov$endpoint
#     )
#
#   list(
#     caid = aka_caid$temporal,
#     care = aka_care$temporal,
#     open = aka_open$temporal,
#     hgov = aka_hgov$temporal
#   )
#
#   list(
#     caid = grp_caid,
#     care = grp_care,
#     open = grp_open,
#     prov = grp_prov
#   )
