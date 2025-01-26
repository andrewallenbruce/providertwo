#' Generate API Request "Offset" Sequence
#'
#' @param nobs `<int>` Number of results returned in the API request
#'
#' @param limit `<int>` Maximum number of results an API will return per request
#'
#' @returns `<int>` If `nobs > limit`, an integer sequence beginning at `0` of
#'    length equal to `nobs / limit + 1`. If `nobs <= limit`, the function
#'    simply returns `nobs`.
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
offset_sequence <- \(nobs, limit) {

  check_number_whole(nobs, min = 0)
  check_number_whole(limit, min = 0)

  if (nobs <= limit) return(nobs)

  seq_(
    from = limit,
    to   = ifelse(nobs %% limit == 0,
                  nobs,
                  sum(nobs, limit)),
    by   = limit)
}

# seq_(from = 0, to = sum(100, 10), by = 10)
# seq_(from = 0, to = sum(47984, 5000), by = 5000)
# seq_(from = 0, to = sum(47984, 2000), by = 2000)

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
