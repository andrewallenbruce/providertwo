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
get_members <- function(x) {
  purrr::map(x, function(i) S7::prop(i, "members"))
}

#' @autoglobal
#' @noRd
set_members <- function(x, i) {
  S7::set_props(x, member_of = i)
}

#' @autoglobal
#' @noRd
map_members <- function(x, index, name) {
  purrr::map2(x[index], name, set_members)
}

#' @autoglobal
#' @noRd
eval_groups <- function(x) {
  rlang::set_names(map_eval(x), paste0("g", seq_along(x)))
}

#' @autoglobal
#' @noRd
eval_params <- function(mods, bare) {
  map_eval(rlang::list2(!!!mods, !!!as_default_mod(bare)))
}
#' @autoglobal
#' @noRd
group_index <- function(g, p) {
  get_members(g) |>
    purrr::map(function(x)
      rlang::names2(p) %iin% x)
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
flatten_query <- function(x) {
  # purrr::map(x, paste0, collapse = "&")
  purrr::map(x, function(x)
    paste0(x, collapse = "&")) |>
    unlist(use.names = FALSE) |>
    paste0(collapse = "&")
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
is_junc <- function(x) {
  rlang::is_call(x, name = c("and", "or"))
}

#' @autoglobal
#' @noRd
is_year <- function(x, negate = FALSE) {
  x[cheapr::which_(rlang::names2(x) == "year", invert = negate)]
}

#' @autoglobal
#' @noRd
is_bare <- function(x) {
  !is_junc(x) & !is_mod(x)
}
