#' @autoglobal
#' @noRd
match_names <- function(x, aka) {
  collapse::fmatch(x, table = rlang::names2(aka))
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
yank <- function(x, ..., .def = NULL) {
  purrr::pluck(x, 1, ..., .default = .def)
}

#' @autoglobal
#' @noRd
`yank<-` <- function(x, value) {
  purrr::pluck(x, 1) <- value
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
