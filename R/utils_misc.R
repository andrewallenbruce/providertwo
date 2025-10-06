options(fastplyr.inform = FALSE)

#' @autoglobal
#' @noRd
uuid_from_url <- function(x) {
  stringi::stri_extract(
    x,
    regex = paste(
      "(?:[0-9a-fA-F]){8}",
      "(?:[0-9a-fA-F]){4}",
      "(?:[0-9a-fA-F]){4}",
      "(?:[0-9a-fA-F]){4}",
      "(?:[0-9a-fA-F]){12}",
      sep = "-?"
    )
  )
}

#' @autoglobal
#' @noRd
set_along <- function(x) {
  rlang::set_names(seq_along(x), rlang::names2(x))
}

#' @autoglobal
#' @noRd
empty <- function(x) {
  rlang::is_empty(x)
}

#' @autoglobal
#' @noRd
`%0%` <- function(x, y) {
  if (rlang::is_empty(x)) y else x
}

#' @autoglobal
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' @autoglobal
#' @noRd
`%|||%` <- function(x, y) {
  if (!is.null(x)) y else NULL
}

#' @autoglobal
#' @noRd
`%|%` <- function(x, y) {
  if (is.na(x)) y else x
}

#' @autoglobal
#' @noRd
names_map <- function(x, f, ..., .nm = x) {
  purrr::map(.x = x, .f = f, ...) |>
    rlang::set_names(nm = .nm)
}

#' @autoglobal
#' @noRd
map_eval <- function(x, ...) {
  purrr::map(.x = x, .f = eval, ...)
}

#' @autoglobal
#' @noRd
flist <- function(...) {
  fastplyr::list_tidy(...)
}

#' @autoglobal
#' @noRd
ffill <- function(x,
                  ...,
                  by = NULL,
                  cols = NULL,
                  direction = "forwards",
                  limit = Inf,
                  new_names = "{.col}") {
  fastplyr::f_fill(
    .data = x,
    ...,
    .by = by,
    .cols = cols,
    .direction = direction,
    .fill_limit = limit,
    .new_names = new_names
  )
}

#' @autoglobal
#' @noRd
fnest <- function(x,
                  ...,
                  add  = FALSE,
                  by   = NULL,
                  cols = NULL) {
  fastplyr::f_nest_by(
    .data = x,
    ...,
    .add = add,
    .by = by,
    .cols = cols) |>
    fastplyr::f_ungroup() |>
    collapse::rnm(endpoints = "data")
}

#' @autoglobal
#' @noRd
yank <- function(x, ..., .def = NULL) {
  pluck(x, 1, ..., .default = .def)
}

#' @autoglobal
#' @noRd
`yank<-` <- function(x, value) {
  pluck(x, 1) <- value
  x
}

#' @autoglobal
#' @noRd
pdetect <- function(x, p, n = FALSE, ci = FALSE) {
  stringi::stri_detect_regex(
    str     = x,
    pattern = p,
    negate  = n,
    case_insensitive = ci
  )
}

#' @autoglobal
#' @noRd
subset_detect <- function(i, j, p, n = FALSE, ci = FALSE) {
  collapse::sbt(i, pdetect(
    x = i[[rlang::ensym(j)]],
    p = p,
    n = n,
    ci = ci
  ))
}

#' @autoglobal
#' @noRd
ss_title <- function(x, re, n = FALSE) {
  subset_detect(i = x, j = title, p = re, n = n)
}

#' @autoglobal
#' @noRd
extract_year <- function(x) {
  as.integer(stringi::stri_extract_first_regex(x, "[12]{1}[0-9]{3}"))
}

#' @autoglobal
#' @noRd
this_year <- function() {
  as.numeric(substr(Sys.Date(), 1, 4))
}

#' @autoglobal
#' @noRd
qmatch <- function(a, b) {
  collapse::fmatch(x = a, table = b, nomatch = 0L, overid = 2)
}

#' @autoglobal
#' @noRd
date_year <- function(x) {
  as.integer(substr(x, 1, 4))
}

#' @autoglobal
#' @noRd
gdetect <- function(str, pt, ...) {
  grepl(x       = str,
        pattern = pt,
        perl    = TRUE,
        ...)
}

#' @autoglobal
#' @noRd
greplace <- function(str, pt, rp, ...) {
  gsub(
    x           = str,
    pattern     = pt,
    replacement = rp,
    perl        = TRUE,
    ...
  )
}

#' @autoglobal
#' @noRd
gremove <- function(str, pt, ...) {
  gsub(
    x           = str,
    pattern     = pt,
    replacement = "",
    perl        = TRUE,
    ...
  )
}

#' @autoglobal
#' @noRd
gextract <- function(x, pt, n = FALSE, ...) {
  stringi::stri_extract_first_regex(str = x, pattern = pt, ...)
}

#' @autoglobal
#' @noRd
rm_nonascii <- function(x) {
  gremove(x, "[^\x20-\x7E]")
}

#' @autoglobal
#' @noRd
rm_space <- function(x) {
  greplace(x, "  ", " ")
}

#' @autoglobal
#' @noRd
rm_quotes <- function(x) {
  gremove(x, "[\"']")
}

#' @autoglobal
#' @noRd
clean_title <- function(x) {
  rm_nonascii(x) |>
    rm_quotes() |>
    trimws() |>
    rm_space()
}

#' @autoglobal
#' @noRd
join_on <- function(x, y, on) {
  collapse::join(x, y, on, verbose = 0, multiple = TRUE)
}

#' @autoglobal
#' @noRd
join_on_title <- function(a, b) {
  join_on(x = a, y = b, on = "title")
}

#' @autoglobal
#' @noRd
str_look <- function(pattern, look) {
  switch(
    match.arg(look, c("ahead", "behind")),
    ahead  = glue::glue("(?<={pattern}).*$"),
    behind = glue::glue("^.*(?={pattern})")
  ) |>
    as.character()
}

#' @autoglobal
#' @noRd
str_look_detect <- function(x, pattern, look) {
  str_look(pattern, look) |>
    grepl(x, perl = TRUE)
}

#' @autoglobal
#' @noRd
str_look_replace <- function(x, pattern, look, replacement) {
  str_look(pattern, look) |>
    gsub(replacement = replacement, x, perl = TRUE)
}

#' @autoglobal
#' @noRd
str_look_remove <- function(x, pattern, look) {
  str_look_replace(x, pattern, look, replacement = "")
}

#' @autoglobal
#' @noRd
str_look_extract <- function(x, pattern, look) {
  gextract(x, pattern = str_look(pattern, look))
}

#' @autoglobal
#' @noRd
delist <- function(x) {
  unlist(x, use.names = FALSE)
}

#' @autoglobal
#' @noRd
as_date <- function(x, ..., fmt = "%Y-%m-%d") {
  as.Date(x, ..., format = fmt)
}

#' ISO 8601 Recurring Time Intervals
#' @source [DCAT Schema: accrualPeriodicity](https://resources.data.gov/resources/dcat-us/#accrualPeriodicity)
#' @param x `<chr>` vector of ISO8601 recurrence rules
#' @returns `<chr>` vector of human-readable recurrence rule descriptions
#' @examplesIf rlang::is_interactive()
#' accrualPeriodicity = c(
#'   "R/PT1S",   "R/PT1H",  "R/P1D", "R/P3.5D",
#'   "R/P0.33W", "R/P0.5W", "R/P1W", "R/P2W",
#'   "R/P0.33M", "R/P0.5M", "R/P1M", "R/P2M",
#'   "R/P3M",    "R/P4M",   "R/P6M", "R/P1Y",
#'   "R/P2Y",    "R/P3Y",   "R/P4Y", "R/P10Y")
#' fmt_periodicity(accrualPeriodicity)
#' @section References:
#' - [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601)
#' - [ISO 8601 Repeating_intervals](https://en.wikipedia.org/wiki/ISO_8601#Repeating_intervals)
#' - [Recurring Time Intervals](https://sentenz.github.io/convention/convention/iso-8601/#19-recurring-time-intervals)
#' @autoglobal
#' @keywords internal
#' @noRd
fmt_periodicity <- function(x) {
  cheapr::val_match(
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
