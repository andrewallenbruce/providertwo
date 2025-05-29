#' @autoglobal
#' @keywords internal
#' @noRd
select_alias <- function(x, alias) {
  subset_detect(x, title, alias)
}

#' @autoglobal
#' @keywords internal
#' @noRd
extract_year <- function(x) {
  as.integer(stri_extract_first_regex(x, "[12]{1}[0-9]{3}"))
}

#' @autoglobal
#' @keywords internal
#' @noRd
gdetect <- function(str, pt, ...) {
  grepl(x = str, pattern = pt, perl = TRUE, ...)
}

#' @autoglobal
#' @keywords internal
#' @noRd
greplace <- function(str, pt, rp, ...) {
  gsub(x = str, pattern = pt, replacement = rp, perl = TRUE, ...)
}

#' @autoglobal
#' @keywords internal
#' @noRd
gremove <- function(str, pt, ...) {
  gsub(x = str, pattern = pt, replacement = "", perl = TRUE, ...)
}

#' @autoglobal
#' @keywords internal
#' @noRd
remove_non_ascii <- function(x) {
  # x[!stri_detect_regex(x, "[^[:ascii:]]")]
  gsub("[^\x20-\x7E]", "", x, perl = TRUE)
}

#' @autoglobal
#' @keywords internal
#' @noRd
join_on_title <- function(a, b) {
  join(x = a, y = b, on = "title", verbose = 0)
}

#' @autoglobal
#' @keywords internal
#' @noRd
str_look <- function(pattern, look) {
  switch(
    match.arg(look, c("ahead", "behind")),
    ahead  = glue("(?<={pattern}).*$"),
    behind = glue("^.*(?={pattern})")
  ) |>
    as.character()
}

#' @autoglobal
#' @keywords internal
#' @noRd
str_look_detect <- function(x, pattern, look) {
  str_look(pattern, look) |>
    grepl(x, perl = TRUE)
}

#' @autoglobal
#' @keywords internal
#' @noRd
str_look_replace <- function(x, pattern, look, replacement) {
  str_look(pattern, look) |>
    gsub(replacement = replacement, x, perl = TRUE)
}

#' @autoglobal
#' @keywords internal
#' @noRd
str_look_remove <- function(x, pattern, look) {
  str_look_replace(x, pattern, look, replacement = "")
}

#' @autoglobal
#' @keywords internal
#' @noRd
str_look_extract <- function(x, pattern, look) {
  stri_extract_first_regex(x, pattern = str_look(pattern, look))
}

#' @autoglobal
#' @keywords internal
#' @noRd
flatten_column <- function(i) {
  map_chr(i, function(x) paste0(delist(x), collapse = ", "))
}

#' @autoglobal
#' @keywords internal
#' @noRd
get_data_elem <- function(x) {
  delist(map(x, function(i) get_elem(as.list(i), "data")))
}

#' @autoglobal
#' @keywords internal
#' @noRd
delist_elem <- function(x, el) {
  delist(get_elem(x, el, DF.as.list = TRUE))
}

#' @autoglobal
#' @keywords internal
#' @noRd
smush_elem <- function(i, el) {
  map_chr(get_elem(i, el), function(x) paste0(x, collapse = ", "))
}

#' @autoglobal
#' @keywords internal
#' @noRd
pdetect <- function(x, p, n = FALSE, ci = FALSE) {
  stri_detect_regex(str     = x,
                    pattern = p,
                    negate  = n,
                    case_insensitive = ci)
}

#' @autoglobal
#' @keywords internal
#' @noRd
subset_detect <- function(i, j, p, n = FALSE, ci = FALSE) {
  sbt(i, pdetect(x = i[[ensym(j)]], p = p, n = n, ci = ci))
}

#' @autoglobal
#' @keywords internal
#' @noRd
empty <- function(x) vec_is_empty(x)

#' @autoglobal
#' @keywords internal
#' @noRd
delist <- function(x) unlist(x, use.names = FALSE)

#' @autoglobal
#' @keywords internal
#' @noRd
as_date <- function(x, ..., fmt = "%Y-%m-%d") as.Date(x, ..., format = fmt)

#' @autoglobal
#' @noRd
roundup <- function(x, d = 2) {
  d  <- 10^d
  z  <- abs(x) * d
  z  <- z + 0.5 + sqrt(.Machine[["double.eps"]])
  z  <- trunc(z)
  z  <- z / d
  z * sign(x)
}
