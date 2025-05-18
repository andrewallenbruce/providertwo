# urls <- c(
# "https://data.cms.gov/provider-data/api/1/datastore/query/",
# "https://openpaymentsdata.cms.gov/api/1/datastore/query/",
# "https://data.medicaid.gov/api/1/datastore/query/",
# "https://data.healthcare.gov/api/1/datastore/query/",
# "https://data.cms.gov/data-api/v1/dataset/1cd9eded-d2c9-4215-a064-aac6dae3b714/data-viewer",
# "https://data.cms.gov/data-api/v1/dataset/5be87981-41ad-41bc-964e-af5cbf22d5af/data",
# "https://data")
# map_vec(urls, identifier_type)
#' @autoglobal
#' @noRd
identifier_type <- function(x) {

  api <- case(
    grepl("data.cms.gov/provider-data", x, perl = TRUE) ~ "pro",
    grepl("openpaymentsdata.cms.gov", x, perl = TRUE) ~ "open",
    grepl("data.medicaid.gov", x, perl = TRUE) ~ "caid",
    grepl("data.healthcare.gov", x, perl = TRUE) ~ "hgov",
    grepl("data.cms.gov/data-api", x, perl = TRUE) ~ "care",
    .default = NA_character_
  )

  if (is_na(api) || api != "care") return(api)

  case(endsWith(x, "viewer") ~ "care_endpoint",
       endsWith(x, "data") ~ "care_temporal")
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
greplace <- \(str, pt, rp, ...) {
  gsub(x = str, pattern = pt, replacement = rp, perl = TRUE, ...)
}

#' @autoglobal
#' @noRd
ndigits <- function(x) {
  stopifnot("x must be an integer" = is.integer(x))
  ceiling(log10(x))
}

#' @autoglobal
#' @noRd
thresh <- function(n, threshold) {
  cheapr_if_else(n > threshold, threshold, n)
}

#' @autoglobal
#' @noRd
make_address <- function(a1, a2) {
  cheapr_if_else(!is_na(a2), paste(a1, a2), a1)
}

#' @autoglobal
#' @noRd
remove_non_ascii <- function(x) {
  # x[!stri_detect_regex(x, "[^[:ascii:]]")]
  gsub("[^\x20-\x7E]", "", x, perl = TRUE)
}

#' @autoglobal
#' @noRd
detect_non_ascii <- function(x) {
  grepl("[^\x20-\x7E]", x, perl = TRUE)
}

#' @autoglobal
#' @noRd
join_on_title <- function(a, b) {
  join(x = a, y = b, on = "title", verbose = 0)
}

#' @autoglobal
#' @noRd
flatten_column <- function(i) {
  map_chr(i, function(x) paste0(delist(x), collapse = ", "))
}

#' @autoglobal
#' @noRd
map_na_if <- function(i) {
  purrr::modify_if(
    i,
    is.character, function(x)
      na_if(x, y = ""))
}

#' @autoglobal
#' @noRd
get_data_elem <- function(x) {
  delist(map(x, \(i) get_elem(as.list(i), "data")))
}

#' @autoglobal
#' @noRd
delist_elem <- function(x, el) {
  delist(get_elem(x, el, DF.as.list = TRUE))
}

#' @autoglobal
#' @noRd
smush_elem <- function(i, el) {
  map_chr(get_elem(i, el), \(x) paste0(x, collapse = ", "))
}

#' @autoglobal
#' @noRd
clean_names <- function(x) {
  gsub("\\(|\\)", "",
       gsub("\\s|-", "_", tolower(x), perl = TRUE),
       perl = TRUE)
}

#' @autoglobal
#' @noRd
set_clean <- function(i, x) {
  set_names(i, clean_names(x))
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

#' @autoglobal
#' @noRd
select_alias <- function(x, alias) {
  subset_detect(x, title, alias)
}

#' @noRd
empty <- function(x) vec_is_empty(x)

#' @autoglobal
#' @noRd
if_empty_null <- function(x) if (empty(x)) NULL else x

#' @autoglobal
#' @noRd
na_if <- function(x, y = "") {
  vctrs::vec_slice(x, vec_in(x, y, needles_arg = "x", haystack_arg = "y")) <- NA
  x
}

#' @noRd
null_to_na <- function(x) if (is.null(x)) NA_character_ else x

#' @noRd
na <- function(x) is_na(x)

#' @noRd
not_na <- function(x) !is_na(x)

#' @noRd
delist <- function(x) unlist(x, use.names = FALSE)

#' @noRd
null <- function(x) is.null(x)

#' @noRd
not_null <- function(x) !is.null(x)

#' @noRd
true <- function(x) isTRUE(x)

#' @noRd
false <- function(x) isFALSE(x)

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
