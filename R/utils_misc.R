#' Generate API Request "Offset" Sequence
#'
#' @param n     `<int>` Number of results returned in an API request
#'
#' @param limit `<int>` API rate limit, i.e. the maximum number of results an
#'                      API will return per request.
#'
#' @returns `<int>` If `n <= limit`, simply returns `n`. If `n > limit`, an
#'                  integer sequence is returned, beginning at `0` and of
#'                  length equal to `ceiling(n / limit)`.
#'
#' @examples
#' offset_seq(100, 10)
#' offset_size(100, 10)
#'
#' offset_seq(10, 100)
#' offset_size(10, 100)
#'
#' offset_seq(47984, 5000)
#' offset_size(47984, 5000)
#'
#' offset_seq(147984, 2000)
#' offset_size(147984, 2000)
#' @name offset
NULL

#' @rdname offset
#' @autoglobal
#' @export
offset_seq <- function(n, limit) {

  check_number_whole(n, min = 0)
  check_number_whole(limit, min = 1)

  if (n <= limit) return(n)

  seq_(from = 0L, to = n, by = limit)
}

#' @rdname offset
#' @autoglobal
#' @export
offset_size <- function(n, limit) {

  check_number_whole(n, min = 0)
  check_number_whole(limit, min = 1)

  if (n <= limit) return(1L)

  seq_size(from = 0L, to = n, by = limit)
}

#' Flatten List Column
#'
#' @param i `<list>` list column
#'
#' @returns `<chr>` comma separated vector
#'
#' @autoglobal
#' @noRd
flatten_column <- function(i) {
  map_chr(i, function(x)
    paste0(delist(x), collapse = ", "))
}

#' Handle NAs
#'
#' @param x `<data.frame>`
#'
#' @returns `<data.frame>`
#'
#' @autoglobal
#' @noRd
handle_na <- function(x) {
  remove_all_na(map_if(x, is.character, function(x)
    na_if(x, y = "")))
}

#' Vectorized `na_if`
#'
#' @param x `<list>` list to handle
#'
#' @returns `<list>` list with NAs handled
#'
#' @autoglobal
#' @noRd
map_na_if <- function(x) {
  map_if(x, is.character, function(x)
    na_if(x, y = ""))
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
#'
#' @param i `<chr>` vector to parse; format: "YYYY-MM-DD"
#'
#' @returns `<chr>` parsed ISOdate vector
#'
#' @autoglobal
#' @noRd
as_fda_date <- function(i) {
  delist(map(i, function(x)
    paste0(
      substr(x, 1, 4), substr(x, 5, 6), substr(x, 7, 8), collapse = "-"
    )))
}

#' Detect by Regex
#'
#' @param x `<chr>` vector to search
#' @param p `<chr>` regular expression pattern
#' @param n `<lgl>` negate
#'
#' @returns `<lgl>` logical vector
#'
#' @autoglobal
#' @noRd
pdetect <- function(x, p, n = FALSE) {
  stri_detect_regex(str     = x,
                    pattern = p,
                    negate  = n)
}

#' Subset by Regex
#'
#' @param i `<data.frame>` to search
#' @param j `<chr>` vector to detect
#' @param p `<chr>` regular expression pattern
#' @param n `<lgl>` negate
#'
#' @returns `<data.frame>` subsetted data.frame
#'
#' @autoglobal
#' @noRd
subset_detect <- function(i, j, p, n = FALSE) {
  sbt(i, pdetect(x = i[[ensym(j)]], p = p, n = n))
}

#' Select by Alias
#' @autoglobal
#' @noRd
select_alias <- function(x, alias) {
  subset_detect(x, title, alias)
}

#' Get List Element Named "data"
#'
#' @param x `<list>` list to get element from
#'
#' @returns `<list>` list with element
#'
#' @autoglobal
#' @noRd
get_data_elem <- function(x) {
  delist(map(x, function(i)
    get_elem(as.list(i), "data")))
}

#' Get List Element
#'
#' @param x `<list>` list to get element from
#' @param el `<chr>` element to get
#'
#' @returns `<list>` list with element
#'
#' @autoglobal
#' @noRd
delist_elem <- function(x, el) {
  delist(get_elem(x, el, DF.as.list = TRUE))
}

#' Get List Element
#'
#' @param i `<list>` list to get element from
#' @param el `<chr>` element to get
#'
#' @returns `<list>` list with element
#'
#' @autoglobal
#' @noRd
smush_elem <- function(i, el) {
  map_chr(get_elem(i, el), function(x)
    sf_smush(x, sep = ", "))
}

#' Concatenate `contactPoint` column
#'
#' @param x `<list>` `contactPoint` column
#'
#' @returns `<chr>` vector
#'
#' @autoglobal
#' @noRd
reduce_contact <- function(x) {
  x <- delist(get_elem(x, "^has", regex = TRUE)) |>
    set_names(delist(get_elem(x, "fn")))

  as.character(glue("{names(x)} ({x})"))
}

#' Check if a property is empty
#'
#' @param obj `<S7_object>` to check
#' @param nm `<chr>` property name
#'
#' @returns `<lgl>` `TRUE` if empty, `FALSE` otherwise
#'
#' @autoglobal
#' @noRd
prop_empty <- function(obj, nm) {
  check_is_S7(obj)
  empty(prop(obj, nm))
}

#' Print a named list
#'
#' @param ls     `<list>` to print
#' @param prefix `<chr>` to prepend to each line
#'
#' @returns `<list>` invisibly
#'
#' @examplesIf rlang::is_interactive()
#' print_list(list(a = 1, b = 2, c = 3))
#'
#' @autoglobal
#' @noRd
print_list <- function(ls, prefix = "") {

  if (length(ls) == 0) cat("<empty>\n")

  ns <- names(ls)

  if (length(ns) != length(ls)) stop("all elements must be named")

  ls <- lapply(ls, as.character)

  cat(sprintf("%s%s : %s", prefix, format(ns), ls), sep = "\n")

  invisible(ls)
}

#' Format temporal string
#'
#' @param x `<chr>` string to format
#'
#' @returns `<chr>` formatted string
#'
#' @autoglobal
#' @noRd
main_temp <- function(x) {
  gsub("/", paste0(" ", cli::symbol$bullet, " "), x, perl = TRUE)
}

#' Clean column names
#'
#' @param x `<chr>` column name
#'
#' @returns `<chr>` cleaned column name
#'
#' @autoglobal
#' @noRd
clean_names <- function(x) {
  gsub(" ", "_", tolower(x), perl = TRUE)
  }

#' Set clean column names
#'
#' @param i `<chr>` vector to be named
#'
#' @param x `<chr>` vector of column names
#'
#' @returns `<chr>` vector with clean column names
#'
#' @autoglobal
#' @noRd
set_clean <- function(i, x) {
  set_names(i, clean_names(x))
}

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
#' recode_iso8601(accrualPeriodicity)
#'
#' @section References:
#'
#' - [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601)
#' - [ISO 8601 Repeating_intervals](https://en.wikipedia.org/wiki/ISO_8601#Repeating_intervals)
#' - [Recurring Time Intervals](https://sentenz.github.io/convention/convention/iso-8601/#19-recurring-time-intervals)
#'
#' @autoglobal
#' @noRd
recode_iso8601 <- function(x) {
  nswitch(
    x,
    "R/P10Y",   "Decennially [R/P10Y]",
    "R/P4Y",    "Quadrennially [R/P4Y]",
    "R/P3Y",    "Triennially [R/P3Y]",
    "R/P2Y",    "Biennially [R/P2Y]",
    "R/P1Y",    "Annually [R/P1Y]",
    "R/P6M",    "Biannually [R/P6M]",
    "R/P4M",    "Triannually [R/P4M]",
    "R/P3M",    "Quarterly [R/P3M]",
    "R/P2M",    "Bimonthly [R/P2M]",
    "R/P1M",    "Monthly [R/P1M]",
    "R/P0.5M",  "Biweekly [R/P0.5M]",
    "R/P2W",    "Biweekly [R/P2W]",
    "R/P0.33M", "Three Times a Month [R/P0.33M]",
    "R/P1W",    "Weekly [R/P1W]",
    "R/P0.5W",  "Twice a Week [R/P0.5W]",
    "R/P3.5D",  "Twice a Week [R/P3.5D]",
    "R/P0.33W", "Three Times a Week [R/P0.33W]",
    "R/P1D",    "Daily [R/P1D]",
    "R/PT1H",   "Hourly [R/PT1H]",
    "R/PT1S",   "Continuously [R/PT1S]",
    default = NA_character_,
    nThread = 4L
  )
}

#' Roxygenise ISO 8601 Recurring Time Intervals
#'
#' @param x `<chr>` vector of ISO8601 recurrence rules
#'
#' @autoglobal
#' @noRd
roxy8601 <- function(x) {
  nswitch(
    x,
    "R/P10Y",   "Decennially (R/P10Y)",
    "R/P4Y",    "Quadrennially (R/P4Y)",
    "R/P3Y",    "Triennially (R/P3Y)",
    "R/P2Y",    "Biennially (R/P2Y)",
    "R/P1Y",    "Annually (R/P1Y)",
    "R/P6M",    "Biannually (R/P6M)",
    "R/P4M",    "Triannually (R/P4M)",
    "R/P3M",    "Quarterly (R/P3M)",
    "R/P2M",    "Bimonthly (R/P2M)",
    "R/P1M",    "Monthly (R/P1M)",
    "R/P0.5M",  "Biweekly (R/P0.5M)",
    "R/P2W",    "Biweekly (R/P2W)",
    "R/P0.33M", "Three Times a Month (R/P0.33M)",
    "R/P1W",    "Weekly (R/P1W)",
    "R/P0.5W",  "Twice a Week (R/P0.5W)",
    "R/P3.5D",  "Twice a Week (R/P3.5D)",
    "R/P0.33W", "Three Times a Week (R/P0.33W)",
    "R/P1D",    "Daily (R/P1D)",
    "R/PT1H",   "Hourly (R/PT1H)",
    "R/PT1S",   "Continuously (R/PT1S)",
    default = NA_character_,
    nThread = 4L
  )
}
