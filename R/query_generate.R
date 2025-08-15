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
generate_query <- function(x = NULL, is_care = FALSE) {
  if (is_null(x) || is_empty(x)) return(NULL)
  set_names(`if`(is_care, query_care(x), query_default(x)), names2(x))
}

#' @autoglobal
#' @noRd
collapse_query <- function(url, params = NULL) {
  if (is_null(params) || is_empty(params)) return(url)
  paste0(url, "&", paste0(unlist(params, use.names = FALSE), collapse = "&"))
}
