#' @noRd
#' @autoglobal
class_query <- S7::new_class(
  name       = "class_query",
  package    = NULL,
  properties = list(
    input    = S7::class_list,
    params   = S7::class_list,
    groups   = S7::class_list
  )
)

#' Create a Query Object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Named conditions where the names are API fields.
#' @returns S7 `<class_query>` object.
#' @name query
NULL

#' @rdname query
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
      ))
  )
}

#' @rdname query
#' @examples
#' query2(
#'   first_name  = starts_with("And"),
#'   middle_name = NULL,
#'   last_name   = contains("J"),
#'   state       = any_of(c("CA", "GA", "NY")),
#'   state_own   = c("GA", "MD"),
#'   npi         = npi_ex$k,
#'   ccn         = "01256",
#'   rate        = between(0.45, 0.67),
#'   year        = 2014:2025,
#'   or("first_name", "last_name"))
#' @autoglobal
#' @export
query2 <- function(...) {
  x <- list(
    groups = purrr::keep(rlang::enexprs(...), \(x) is_junc(x)),
    params = purrr::discard(rlang::enexprs(...), \(x) is_junc(x))
  )

  class_query(
    input  = purrr::compact(rlang::enexprs(...)),
    params = purrr::map(x$params, eval),
    groups = purrr::map(x$groups, eval)
  )
}

#' @autoglobal
#' @noRd
query3 <- function(...) {
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
