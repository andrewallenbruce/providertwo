#' Generate API Request "Offset" Sequence
#'
#' @param n     `<int>` Number of results returned in an API request
#'
#' @param limit `<int>` API rate limit, i.e. the maximum number of results an
#'                      API will return per request.
#'
#' @param start `<int>` Offset start; either `0` (default) or `limit`
#'
#' @returns `<int>` If `n <= limit`, simply returns `n`. If `n > limit`, an
#'                  integer sequence is returned, beginning at `start` and of
#'                  length equal to `n / limit`.
#'
#' @examples
#' offset_sequence(100, 10)
#' offset_length(100, 10)
#'
#' offset_sequence(10, 100)
#' offset_length(10, 100)
#'
#' offset_sequence(47984, 5000)
#' offset_length(47984, 5000)
#'
#' offset_sequence(147984, 2000)
#' offset_length(147984, 2000)
#'
#' @name offset
NULL

#' @rdname offset
#' @autoglobal
#' @keywords internal
#' @export
offset_sequence <- function(n, limit, start = 0) {

  check_number_whole(n, min = 0)
  check_number_whole(limit, min = 1)
  check_number_whole(start, min = 0)

  if (n <= limit) return(n)

  seq_(from = start, to = n, by = limit)
}

#' @rdname offset
#' @autoglobal
#' @keywords internal
#' @export
offset_length <- function(n, limit, start = 0) {

  check_number_whole(n, min = 0)
  check_number_whole(limit, min = 1)
  check_number_whole(start, min = 0)

  if (n <= limit) return(1L)

  length(seq_(from = start, to = n, by = limit))
}

# Incorrect
# seq_(from = start, to = ifelse(n %% limit == 0, n, sum(n, limit)), by = limit)

#' @rdname offset
#' @autoglobal
#' @keywords internal
#' @export
seq_along0 <- function(x) {
  seq_along(x) - 1
}

#' Flatten Column
#' @param i `<list>` list to flatten
#' @returns `<chr>` flattened list
#' @autoglobal
#' @keywords internal
#' @export
flatten_column <- function(i) {
  map_chr(i, function(x) paste0(delist(x), collapse = ", "))
}

#' Handle NAs
#' @param x `<list>` list to handle
#' @returns `<list>` list with NAs handled
#' @autoglobal
#' @keywords internal
#' @export
handle_na <- function(x) {
  remove_all_na(map_if(x, is.character, function(x) na_if(x, y = "")))
}

#' Vectorized `na_if`
#' @param x `<list>` list to handle
#' @returns `<list>` list with NAs handled
#' @autoglobal
#' @keywords internal
#' @export
map_na_if <- function(x) {
  map_if(x, is.character, function(x) na_if(x, y = ""))
}

#' Parse datetime character vectors
#' @param x `<chr>` vector to parse; format: "YYYY-MM-DDTHH:MM:SS"
#' @returns `<chr>` parsed ISOdatetime vector
#' @examples
#' as_datetime("2024-07-29T20:37:53")
#' @seealso [clock::date_time_parse_RFC_3339()]
#' @autoglobal
#' @keywords internal
#' @export
as_datetime <- function(x) {

  ISOdatetime(
    sf_sub(x, 1, 4),
    sf_sub(x, 6, 7),
    sf_sub(x, 9, 10),
    sf_sub(x, 12, 13),
    sf_sub(x, 15, 16),
    sf_sub(x, 18, 19))

}

#' Parse openFDA date character vectors
#' @param i `<chr>` vector to parse; format: "YYYY-MM-DD"
#' @returns `<chr>` parsed ISOdate vector
#' @autoglobal
#' @keywords internal
#' @export
as_fda_date <- function(i) {
  delist(map(i, function(x)
    paste0(
      sf_sub(x, 1, 4),
      "-",
      sf_sub(x, 5, 6),
      "-",
      sf_sub(x, 7, 8)
    )))
}

#' Detect Regular Expression
#' @param x `<chr>` vector to search
#' @param p `<chr>` regular expression pattern
#' @param n `<lgl>` negate
#' @returns `<lgl>` logical vector
#' @autoglobal
#' @keywords internal
#' @export
detect <- function(x, p, n = FALSE) {
  stri_detect_regex(str     = x,
                    pattern = p,
                    negate  = n)
}

#' Subset by Regular Expression
#' @param i `<data.frame>` to search
#' @param j `<chr>` vector to detect
#' @param p `<chr>` regular expression pattern
#' @param n `<lgl>` negate
#' @returns `<data.frame>` subsetted data.frame
#' @autoglobal
#' @keywords internal
#' @export
subset_detect <- function(i, j, p, n = FALSE) {
  sbt(i,
      detect(x = i[[ensym(j)]],
             p = p,
             n = n))
}

#' Get List Element Named "data"
#' @param x `<list>` list to get element from
#' @returns `<list>` list with element
#' @autoglobal
#' @keywords internal
#' @export
get_data_elem <- function(x) {
  delist(map(x, function(i)
    get_elem(as.list(i), "data")))
}


#' Get List Element
#' @param x `<list>` list to get element from
#' @param el `<chr>` element to get
#' @returns `<list>` list with element
#' @autoglobal
#' @keywords internal
#' @export
delist_elem <- function(x, el) {
  delist(get_elem(x, el, DF.as.list = TRUE))
}

#' Get List Element
#' @param i `<list>` list to get element from
#' @param el `<chr>` element to get
#' @returns `<list>` list with element
#' @autoglobal
#' @keywords internal
#' @export
smush_elem <- function(i, el) {
  map_chr(get_elem(i, el), function(x)
    sf_smush(x, sep = ", "))
}
