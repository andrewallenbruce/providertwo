#' @noRd
#' @autoglobal
not_catalog <- function(obj) {
  purrr::map_lgl(obj, function(x) !S7::S7_inherits(x, class_catalog))
}

#' @noRd
#' @autoglobal
which_not_catalog <- function(obj) {
  collapse::funique(
    purrr::map_chr(
      unname(
        obj[not_catalog(obj)]),
      function(x) pluck(class(x), 1)
    )
  )
}

#' @autoglobal
#' @noRd
bound <- function(lower, upper) {
  check_number_whole(lower, min = 0)
  check_number_whole(upper, min = 1)
  iif(lower > upper, upper, lower, nThread = 4L, tprom = TRUE)
}

#' @autoglobal
#' @noRd
is_complete_with_limit <- function(limit) {
  function(resp)
    length(resp_body_json(resp)$data) < limit
}

#' @autoglobal
#' @noRd
req_perform_iterative_offset <- function(req, limit) {
  # TODO allow switching between different API limits?
  check_number_whole(limit, min = 1, max = 8000)

  req_perform_iterative(
    req,
    next_req        = iterate_with_offset(
      param_name    = "offset",
      start         = 0L,
      offset        = limit,
      resp_complete = is_complete_with_limit(limit)
    )
  )
}

#' @autoglobal
#' @noRd
perform_simple_request <- function(x, ...) {
  x |>
    request() |>
    req_perform() |>
    resp_simple_json(...)
}

#' @autoglobal
#' @noRd
resp_simple_json <- function(resp, ...) {
  resp_body_json(resp, simplifyVector = TRUE, check_type = FALSE, ...)
}

#' @autoglobal
#' @noRd
url_type <- function(x) {
  api <- case(
    grepl("data.cms.gov/provider-data", x, perl = TRUE) ~ "prov",
    grepl("openpaymentsdata.cms.gov",   x, perl = TRUE) ~ "open",
    grepl("data.medicaid.gov",          x, perl = TRUE) ~ "caid",
    grepl("data.healthcare.gov",        x, perl = TRUE) ~ "hgov",
    grepl("data.cms.gov/data-api",      x, perl = TRUE) ~ "care",
    .default = NA_character_
  )

  if (is.na(api)) cli::cli_abort(c("x" = "{.val {x}} not recognized."))
  if (api != "care") return(api)

  case(grepl("/data-viewer?", x, perl = TRUE) ~ "current",
       grepl("/data?",        x, perl = TRUE) ~ "temporal")
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
roxy8601 <- function(x) {
  cheapr::val_match(
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

# endpoint <- function(alias) {
#
#   x   <- alias_lookup(alias)
#
#   cls <- as_function(glue("class_{x$catalog}"))
#
#   pnt <- `if`(x$catalog == "care",
#               as_function(glue("{x$catalog}_{x$point}")),
#               as_function(glue("class_{x$point}")))
#
#
#   cls(access = pnt(
#     identifier = ifelse(x$point == "current", x$identifier, x$endpoints),
#     metadata = get_metadata(x),
#     dimensions = get_dimensions(x)
#   ))
# }

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

#' @autoglobal
#' @noRd
convert_epoch <- function(x) {
  as.Date(as.POSIXct.numeric(as.numeric(x) / 1000L, origin = "1970-01-01"))
}

#' @autoglobal
#' @keywords internal
#' @noRd
make_join_col <- function(x, col) {
  map(x[[ensym(col)]], function(x) get_elem(as.list(x), "data")) |>
    flatten_column() |>
    na_if("")
}

#' @autoglobal
#' @noRd
luhn_check <- function(x) {

  i <- c(1L, 3L, 5L, 7L, 9L)
  d <- cheapr_rev(as.integer(strsplit(delist(as.character(x)), "")[[1]][1:9]))

  d[i] <- d[i] * 2L
  d[i] <- ifelse(d[i] > 9L, d[i] - 9L, d[i])

  d <- sum(d) + 24L
  d <- (ceiling(d / 10) * 10) - d

  identical(paste0(substr(x, 1, 9), d), x)
}
