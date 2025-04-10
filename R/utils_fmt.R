#' @autoglobal
#' @noRd
print_list <- function(ls, prefix = "") {
  if (length(ls) == 0) cat("<empty>\n")

  if (length(names(ls)) != length(ls)) stop("all elements must be named")

  ls <- lapply(ls, as.character)

  cat(sprintf("%s%s : %s", prefix, format(names(ls)), ls), sep = "\n")

  invisible(ls)
}

#' @autoglobal
#' @noRd
fmt_int <- function(x) {
  if (x >= 1e6) return(paste0(round(x / 1e6, 1), "M"))
  if (x >= 1e3) return(paste0(round(x / 1e3, 0), "K"))
  as.character(x)
}

#' @noRd
fmt_num <- function(x) prettyNum(x, big.mark = ",")

# cli_results(n = 1000, limit = 10, end = "Profile Summary", api = "Open Payments")
#' @autoglobal
#' @noRd
cli_results <- function(n, limit, end, api) {
  cli_inform(
    c(
      "{.pkg {symbol$square_small_filled}} {end} {.kbd {api}}",
      "{.pkg {symbol$square_small_filled}} {n} RW{?S} {.kbd {offset_size(n, limit)} PG{?S}}",
      "{.pkg {symbol$square_small_filled}} {n} RW{?S} {.kbd {offset_size(n, limit)} PG{?S}}"
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
