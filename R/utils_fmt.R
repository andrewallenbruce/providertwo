#' @autoglobal
#' @noRd
fmt_entity <- function(x, type = c("int", "chr")) {

  type <- match.arg(type, c("int", "chr"))

  switch(
    type,
    int = val_match(x, 1 ~ "I", 2 ~ "O"),
    chr = val_match(x, "NPI-1" ~ "I", "NPI-2" ~ "O")
  ) |>
    factor_()

}

#' @autoglobal
#' @noRd
fmt_contactpoint <- function(x) {
  x <- delist(get_elem(x, "^has", regex = TRUE)) |>
    set_names(delist(get_elem(x, "fn")))

  as.character(glue("{names(x)} ({x})"))
}

#' @autoglobal
#' @noRd
fmt_temporal <- function(x) {
  gsub("/", paste0(" ", cli::symbol$bullet, " "), x, perl = TRUE)
}

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

#' ISO 8601 Recurring Time Intervals
#'
#' @source [DCAT Schema: accrualPeriodicity](https://resources.data.gov/resources/dcat-us/#accrualPeriodicity)
#'
#' @param x `<chr>` vector of ISO8601 recurrence rules
#'
#' @returns `<chr>` vector of human-readable recurrence rule descriptions
#'
#' @examplesIf rlang::is_interactive()
#' accrualPeriodicity = c(
#'   "R/PT1S",   "R/PT1H",  "R/P1D", "R/P3.5D",
#'   "R/P0.33W", "R/P0.5W", "R/P1W", "R/P2W",
#'   "R/P0.33M", "R/P0.5M", "R/P1M", "R/P2M",
#'   "R/P3M",    "R/P4M",   "R/P6M", "R/P1Y",
#'   "R/P2Y",    "R/P3Y",   "R/P4Y", "R/P10Y")
#'
#' fmt_periodicity(accrualPeriodicity)
#'
#' @section References:
#'
#' - [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601)
#' - [ISO 8601 Repeating_intervals](https://en.wikipedia.org/wiki/ISO_8601#Repeating_intervals)
#' - [Recurring Time Intervals](https://sentenz.github.io/convention/convention/iso-8601/#19-recurring-time-intervals)
#'
#' @autoglobal
#' @noRd
fmt_periodicity <- function(x) {
  val_match(
    x,
    "R/P10Y"   ~ "Decennially [R/P10Y]",
    "R/P4Y"    ~ "Quadrennially [R/P4Y]",
    "R/P3Y"    ~ "Triennially [R/P3Y]",
    "R/P2Y"    ~ "Biennially [R/P2Y]",
    "R/P1Y"    ~ "Annually [R/P1Y]",
    "R/P6M"    ~ "Biannually [R/P6M]",
    "R/P4M"    ~ "Triannually [R/P4M]",
    "R/P3M"    ~ "Quarterly [R/P3M]",
    "R/P2M"    ~ "Bimonthly [R/P2M]",
    "R/P1M"    ~ "Monthly [R/P1M]",
    "R/P0.5M"  ~ "Biweekly [R/P0.5M]",
    "R/P2W"    ~ "Biweekly [R/P2W]",
    "R/P0.33M" ~ "Three Times a Month [R/P0.33M]",
    "R/P1W"    ~ "Weekly [R/P1W]",
    "R/P0.5W"  ~ "Twice a Week [R/P0.5W]",
    "R/P3.5D"  ~ "Twice a Week [R/P3.5D]",
    "R/P0.33W" ~ "Three Times a Week [R/P0.33W]",
    "R/P1D"    ~ "Daily [R/P1D]",
    "R/PT1H"   ~ "Hourly [R/PT1H]",
    "R/PT1S"   ~ "Continuously [R/PT1S]",
    .default = x
  )
}

#' @autoglobal
#' @noRd
roxy8601 <- function(x) {
  val_match(
    x,
    "R/P10Y"   ~ "Decennially (R/P10Y)",
    "R/P4Y"    ~ "Quadrennially (R/P4Y)",
    "R/P3Y"    ~ "Triennially (R/P3Y)",
    "R/P2Y"    ~ "Biennially (R/P2Y)",
    "R/P1Y"    ~ "Annually (R/P1Y)",
    "R/P6M"    ~ "Biannually (R/P6M)",
    "R/P4M"    ~ "Triannually (R/P4M)",
    "R/P3M"    ~ "Quarterly (R/P3M)",
    "R/P2M"    ~ "Bimonthly (R/P2M)",
    "R/P1M"    ~ "Monthly (R/P1M)",
    "R/P0.5M"  ~ "Biweekly (R/P0.5M)",
    "R/P2W"    ~ "Biweekly (R/P2W)",
    "R/P0.33M" ~ "Three Times a Month (R/P0.33M)",
    "R/P1W"    ~ "Weekly (R/P1W)",
    "R/P0.5W"  ~ "Twice a Week (R/P0.5W)",
    "R/P3.5D"  ~ "Twice a Week (R/P3.5D)",
    "R/P0.33W" ~ "Three Times a Week (R/P0.33W)",
    "R/P1D"    ~ "Daily (R/P1D)",
    "R/PT1H"   ~ "Hourly (R/PT1H)",
    "R/PT1S"   ~ "Continuously (R/PT1S)",
    .default   = "Unknown"
  )
}

#' Parse datetime
#'
#' @param x `<chr>` vector to parse; format: "YYYY-MM-DDTHH:MM:SS"
#'
#' @returns `<chr>` parsed ISOdatetime vector
#'
#' @examplesIf rlang::is_interactive()
#' as_datetime("2024-07-29T20:37:53")
#'
#' @seealso [clock::date_time_parse_RFC_3339()]
#' @autoglobal
#' @noRd
as_datetime <- function(x) {
  ISOdatetime(
    substr(x, 1, 4),
    substr(x, 6, 7),
    substr(x, 9, 10),
    substr(x, 12, 13),
    substr(x, 15, 16),
    substr(x, 18, 19)
  )
}

#' Parse `openFDA` date character vectors
#' @autoglobal
#' @noRd
as_fda_date <- function(i) {
  delist(map(i, function(x)
    paste0(
      substr(x, 1, 4),
      substr(x, 5, 6),
      substr(x, 7, 8),
      collapse = "-"
    )))
}
