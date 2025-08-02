#' Create a Query Object
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Named conditions where the names are API fields.
#'
#' @returns S7 `<class_query>` object.
#'
#' @examples
#' new_query(
#'   first_name  = starts_with("Andr"),
#'   middle_name = NULL,
#'   last_name   = contains("J"),
#'   state       = any_of(c("CA", "GA", "NY")),
#'   city        = equals(c("Atlanta", "Los Angeles"), negate = TRUE),
#'   state_own   = c("GA", "MD"),
#'   npi         = npi_ex$k,
#'   ccn         = "01256",
#'   rate        = between(0.45, 0.67),
#'   year        = 2014:2025)
#' @autoglobal
#' @export
new_query <- function(...) {
  class_query(
    input  = purrr::compact(rlang::enexprs(...)),
    params = purrr::compact(rlang::dots_list(..., .homonyms = "error"))
  )
}

#' @autoglobal
#' @noRd
query_care <- function(args) {
  purrr::imap(args, function(x, name) {
    v <- `if`(is_modifier(x), x@value, unlist(x, use.names = FALSE))

    c(
      paste0("filter[<<i>>][condition][path]=", name),
      paste0(
        "filter[<<i>>][condition][operator]=",
        `if`(is_modifier(x), toupper(x@operator), "=")
      ),
      `if`(
        length(v) > 1,
        paste0("filter[<<i>>][condition][value][", seq_along(v), "]=", v),
        paste0("filter[<<i>>][condition][value]=", v)
      )
    )
  }) |>
    unname() |>
    purrr::imap(function(x, idx)
      greplace(x, "<<i>>", idx))
}

#' @autoglobal
#' @noRd
query_default <- function(args) {
  purrr::imap(args, function(x, name) {
    v <- `if`(is_modifier(x), x@value, unlist(x, use.names = FALSE))

    c(
      paste0("conditions[<<i>>][property]=", name),
      paste0(
        "conditions[<<i>>][operator]=",
        `if`(is_modifier(x), tolower(gsub("_", "+", x@operator, perl = TRUE)), "=")
      ),
      `if`(
        length(v) > 1,
        paste0("conditions[<<i>>][value][", seq_along(v), "]=", v),
        paste0("conditions[<<i>>][value]=", v)
      )
    )
  }) |>
    unname() |>
    purrr::imap(function(x, idx)
      greplace(x, "<<i>>", idx - 1))
}
