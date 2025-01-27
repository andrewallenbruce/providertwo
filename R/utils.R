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
#'
#' offset_sequence(10, 100)
#'
#' offset_sequence(47984, 5000)
#'
#' offset_sequence(147984, 2000)
#'
#' @autoglobal
#'
#' @export
offset_sequence <- \(n, limit, start = 0) {

  check_number_whole(n, min = 0)
  check_number_whole(limit, min = 0)
  check_number_whole(start, min = 0)

  if (n <= limit) return(n)

  seq_(from = start, to = n, by = limit)
}

# Incorrect
# seq_(from = start, to = ifelse(n %% limit == 0, n, sum(n, limit)), by = limit)
# seq_(from = 0, to = sum(100, 10), by = 10)
# seq_(from = 0, to = sum(47984, 5000), by = 5000)
# seq_(from = 0, to = sum(47984, 2000), by = 2000)

# Correct
# seq_(from = 0, to = 100, by = 10)
# seq_(from = 0, to = 47984, by = 5000)
# seq_(from = 0, to = 47984, by = 2000)

#' Flatten Column
#' @param i `<list>` list to flatten
#' @returns `<chr>` flattened list
#' @autoglobal
#' @noRd
flatten_column <- \(i) {
  map_chr(i, \(x) paste0(delist(x), collapse = ", "))
  }

#' Handle NAs
#' @param x `<list>` list to handle
#' @returns `<list>` list with NAs handled
#' @autoglobal
#' @noRd
handle_na <- \(x) {
  remove_all_na(
    map_if(
      x,
      is.character,
      \(x) na_if(x, y = "")
      )
    )
  }

#' Vectorized na_if
#' @param x `<list>` list to handle
#' @returns `<list>` list with NAs handled
#' @autoglobal
#' @noRd
vna_if <- \(x) {
  map_if(x, is.character, \(x) na_if(x, y = ""))
  }

#' #' @noRd
#' debugme_on <- \() Sys.setenv(DEBUGME = "providertwo")
#'
#' #' @noRd
#' debugme_off <- \() Sys.unsetenv("DEBUGME")
#'
#' #' @noRd
#' is_debuggingme <- \() identical(Sys.getenv("DEBUGME"), "providertwo")
