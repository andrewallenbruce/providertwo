#' @autoglobal
#' @noRd
as_default_mod <- function(x) {

  len <- cheapr::list_lengths(
    purrr::map(x, function(x) as.character(x)[-1])) > 1L

  purrr::map2(x, len, function(i, L)
    str2lang(paste0(if (L) "any_of(" else "equal(", deparse1(i), ")")))
}

#' @autoglobal
#' @noRd
are_empty <- function(x) {
  purrr::map(x, rlang::is_empty) |>
    unlist(use.names = FALSE)
}

#' @autoglobal
#' @noRd
are_not_empty <- function(x) {
  purrr::map(x, Negate(rlang::is_empty)) |>
    unlist(use.names = FALSE)
}

#' @autoglobal
#' @noRd
eval_params <- function(mods, bare) {
  map_eval(rlang::list2(!!!mods, !!!as_default_mod(bare)))
}

#' @autoglobal
#' @noRd
generate_query <- function(x = NULL, is_care = FALSE) {
  if (rlang::is_null(x) || rlang::is_empty(x)) {
    return(NULL)
  }

  rlang::set_names(
    `if`(
      is_care,
      query_care(x),
      query_default2(x)
    ),
    rlang::names2(x)
  )
}

#' @autoglobal
#' @noRd
collapse_query <- function(url, params = NULL) {
  if (rlang::is_null(params) || rlang::is_empty(params)) {
    return(url)
  }

  paste0(
    url,
    "&",
    paste0(
      unlist(params, use.names = FALSE),
      collapse = "&"
    )
  )
}

#' @autoglobal
#' @noRd
total_rows <- function(x) {
  collapse::get_elem(x, "total_rows") |>
    unlist(use.names = FALSE)
}

#' @autoglobal
#' @noRd
found_rows <- function(x) {
  collapse::get_elem(x, "found_rows") |>
    unlist(use.names = FALSE)
}

#' @autoglobal
#' @noRd
is_mod <- function(x) {
  rlang::is_call(
    x,
    name = c(
      "any_of",
      "none_of",
      "equal",
      "not_equal",
      "between",
      "not_between",
      "greater_than",
      "less_than",
      "starts_with",
      "ends_with",
      "contains",
      "like"
    )
  )
}

#' @autoglobal
#' @noRd
is_year <- function(x, negate = FALSE) {
  x[cheapr::which_(rlang::names2(x) == "year", invert = negate)]
}

#' @autoglobal
#' @noRd
is_bare <- function(x) {
  !is_mod(x)
}
