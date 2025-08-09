#' Create a Query Object
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Named conditions where the names are API fields.
#'
#' @returns S7 `<class_query>` object.
#'
#' @examples
#' query(
#'   first_name  = starts_with("And"),
#'   middle_name = NULL,
#'   last_name   = contains("J"),
#'   state       = any_of(c("CA", "GA", "NY")),
#'   state_own   = c("GA", "MD"),
#'   npi         = npi_ex$k,
#'   ccn         = "01256",
#'   rate        = between(0.45, 0.67),
#'   year        = 2014:2025)
#' @autoglobal
#' @export
query <- function(...) {
  class_query(
    input  = purrr::compact(rlang::enexprs(...)),
    params = purrr::compact(
      rlang::dots_list(
        ...,
        .homonyms = "error",
        .named = TRUE,
        .check_assign = TRUE
      )
    )
  )
}

#' @autoglobal
#' @noRd
query2 <- function(...) {
  class_query(
    input  = rlang::enquos(
      ...,
      .homonyms = "error",
      .named = TRUE,
      .ignore_null = "all",
      .check_assign = TRUE
    ),
    params = purrr::compact(
      rlang::dots_list(
        ...,
        .homonyms = "error",
        .named = TRUE,
        .check_assign = TRUE
      )
    )
  )
}

# TODO Explore chunking params with 10+ elements
# Submitting too many params at once can cause API errors
# TODO Consider the case sensitivity of each endpoint's fields' values
# Some are all uppercase and won't match anything but uppercase values

#' @autoglobal
#' @noRd
query_care <- function(args) {

  purrr::imap(args, function(x, N) {

    V <- if (is_modifier(x)) x@value else unlist(x, use.names = FALSE)
    O <- if (is_modifier(x)) toupper(x@operator) else "="
    N <- gsub(" ", "+", N, fixed = TRUE)

    c(paste0("filter[<<i>>][condition][path]=", N),
      paste0("filter[<<i>>][condition][operator]=", O),
      `if`(
        length(V) > 1L,
        paste0("filter[<<i>>][condition][value][", seq_along(V), "]=", V),
        paste0("filter[<<i>>][condition][value]=", V)))
  }) |>
    unname() |>
    purrr::imap(function(x, idx)
      gsub(x           = x,
           pattern     = "<<i>>",
           replacement = idx,
           fixed       = TRUE)
      ) |>
    purrr::map(paste0, collapse = "&")
}

#' @autoglobal
#' @noRd
query_default <- function(args) {

  purrr::imap(args, function(x, N) {

    V <- if (is_modifier(x)) x@value else unlist(x, use.names = FALSE)
    O <- if (is_modifier(x)) tolower(gsub("_", "+", x@operator, fixed = TRUE)) else "="
    N <- gsub(" ", "+", N, fixed = TRUE)

    c(paste0("conditions[<<i>>][property]=", N),
      paste0("conditions[<<i>>][operator]=", O),
      `if`(
        length(V) > 1L,
        paste0("conditions[<<i>>][value][", seq_along(V), "]=", V),
        paste0("conditions[<<i>>][value]=", V)))
  }) |>
    unname() |>
    purrr::imap(function(x, idx)
      gsub(x           = x,
           pattern     = "<<i>>",
           replacement = idx - 1,
           fixed       = TRUE)
      ) |>
    purrr::map(paste0, collapse = "&")
}

#' @autoglobal
#' @noRd
c_match <- function(a, b) {
  collapse::fmatch(x = a, table = b, nomatch = 0L, overid = 2)
}

#' @autoglobal
#' @noRd
match_query <- function(obj, qry) {

  # Remove "year" if present
  param  <- parameters(qry)[setdiff(names(parameters(qry)), "year")]
  pname  <- names(param)

  # TODO Use a modifier on "year" parameter if meant for an API field?
  field <- field_keys(obj)
  clean <- clean_names(field)

  set_names(
    param[c_match(clean, pname)],
    field[sort(c_match(pname, clean))])
}

#' @autoglobal
#' @noRd
generate_query <- function(x = NULL, is_care = FALSE) {
  if (is_null(x) || is_empty(x)) return(NULL)
  set_names(`if`(is_care, query_care(x), query_default(x)), names(x))
}

#' @autoglobal
#' @noRd
collapse_query <- function(url, params = NULL) {
  if (is_null(params) || is_empty(params)) return(url)
  paste0(url, "&", paste0(unlist(params, use.names = FALSE), collapse = "&"))
}
