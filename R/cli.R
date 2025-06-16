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

# cli::boxx(
#   label = openMain("profile_mapping")@description |>
#     strwrap(width = 45, prefix = paste(symbol$upper_block_4, " ")),
#   header = "API: Open Payments",
#   footer = "Covered Recipient Profile Supplement",
#   width = 50,
#   border_style = "round")
#
# writeLines(c(bx, bx))
