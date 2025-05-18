#' @autoglobal
#' @noRd
select_alias <- function(x, alias) {
  subset_detect(x, title, alias)
}

#' @autoglobal
#' @noRd
extract_year <- function(x) {
  as.integer(stri_extract_first_regex(x, "[12]{1}[0-9]{3}"))
}

#' @autoglobal
#' @noRd
grapple <- function(str, pt, ...) {
  grepl(x = str, pattern = pt, perl = TRUE, ...)
}

#' @autoglobal
#' @noRd
greplace <- function(str, pt, rp, ...) {
  gsub(x = str, pattern = pt, replacement = rp, perl = TRUE, ...)
}

#' @autoglobal
#' @noRd
remove_non_ascii <- function(x) {
  # x[!stri_detect_regex(x, "[^[:ascii:]]")]
  gsub("[^\x20-\x7E]", "", x, perl = TRUE)
}

#' @autoglobal
#' @noRd
join_on_title <- function(a, b) {
  join(x = a, y = b, on = "title", verbose = 0)
}

#' @autoglobal
#' @noRd
str_look <- function(pattern, look) {
  switch(
    match.arg(look, c("ahead", "behind")),
    ahead  = glue("(?<={pattern}).*$"),
    behind = glue("^.*(?={pattern})")
  )
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
flatten_column <- function(i) {
  map_chr(i, function(x) paste0(delist(x), collapse = ", "))
}

#' @autoglobal
#' @noRd
get_data_elem <- function(x) {
  delist(map(x, function(i) get_elem(as.list(i), "data")))
}

#' @autoglobal
#' @noRd
delist_elem <- function(x, el) {
  delist(get_elem(x, el, DF.as.list = TRUE))
}

#' @autoglobal
#' @noRd
smush_elem <- function(i, el) {
  map_chr(get_elem(i, el), function(x) paste0(x, collapse = ", "))
}

#' @autoglobal
#' @noRd
pdetect <- function(x, p, n = FALSE, ci = FALSE) {
  stri_detect_regex(str     = x,
                    pattern = p,
                    negate  = n,
                    case_insensitive = ci)
}

#' @autoglobal
#' @noRd
subset_detect <- function(i, j, p, n = FALSE, ci = FALSE) {
  sbt(i, pdetect(x = i[[ensym(j)]], p = p, n = n, ci = ci))
}

#' @noRd
empty <- function(x) vec_is_empty(x)

#' @autoglobal
#' @noRd
if_empty_null <- function(x) if (empty(x)) NULL else x

#' @noRd
not_na <- function(x) !is_na(x)

#' @noRd
delist <- function(x) unlist(x, use.names = FALSE)

#' @noRd
null <- function(x) is.null(x)

#' @noRd
not_null <- function(x) !is.null(x)

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
