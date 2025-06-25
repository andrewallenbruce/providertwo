# cli_results(n = 1000, limit = 10, end = "Profile Summary", api = "Open Payments")
#' @autoglobal
#' @noRd
cli_results <- function(n, limit, end, api) {
  cli_inform(
    c(
      "{.pkg {cli::symbol$square_small_filled}} {end} {.kbd {api}}",
      "{.pkg {cli::symbol$square_small_filled}} {n} RW{?S} {.kbd {offset_size(n, limit)} PG{?S}}"
    )
  )
}

# x <- new_endpoint("profile_mapping")
#
# cli::boxx(
#   label = x@metadata$description |>
#     strwrap(width = 40, initial = paste(cli::symbol$bullet, " ")),
#   header = x@metadata$title,  # "Covered Recipient Profile Supplement",
#   footer = "Open Payments",
#   width = 40,
#   border_style = "round",
#   border_col = "grey")
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
# list_aliases <- function(aka, type) {
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
#
# }
#
# x <- unlist(aka_care$endpoint)
# i <- collapse::radixorder(names(x), sort = TRUE)
#
# cli::cli_alert_info("{.cls class_care} {.field Endpoint} {.emph aliases}")
# glue_col(
#     "{yellow {format(names(x), justify = 'left')}}",
#     " {clisymbols::symbol$bullet} ",
#     "{silver {strtrim(unname(x), width = 60)}}",
#     x = set_names(unname(x)[i], names(x)[i])
#     )
#
# cli::boxx(
#   label = strwrap(x@metadata$description, width = 40, initial = paste(cli::symbol$bullet, " ")),
#   header = x@metadata$title,
#   footer = "Open Payments",
#   width = 40,
#   border_style = "round",
#   border_col = "grey")
