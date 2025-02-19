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

# ex_resource_url   <- "https://data.cms.gov/data-api/v1/dataset-resources/7dcf9ea6-ee2f-4bf1-8b5d-39c18b0e8541"
# ex_identifier_url <- "https://data.cms.gov/data-api/v1/dataset/2457ea29-fc82-48b0-86ec-3b0755de7515/data-viewer"

# debugme_on     <- \() Sys.setenv(DEBUGME = "providertwo-VERBOSE")
# debugme_off    <- \() Sys.unsetenv("DEBUGME")
# is_debuggingme <- \() identical(Sys.getenv("DEBUGME"), "providertwo-VERBOSE")
