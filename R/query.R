# class_query <- S7::new_class(
#   name       = "class_query",
#   package    = NULL,
#   properties = list(
#     input    = S7::new_property(
#       S7::class_list,
#       setter = function(self, value) {
#         self@input <- value
#         self
#       }
#     ),
#     params = S7::new_property(
#       S7::class_list,
#       setter = function(self, value) {
#         self@params <- value
#         self
#       }
#     ),
#     groups = S7::class_list
#   )
# )

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

#' @autoglobal
#' @noRd
params <- S7::new_generic("params", "obj", function(obj) {
  S7::S7_dispatch()
})

S7::method(params, class_query) <- function(obj) {
  S7::prop(obj, "params")
}


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
#'   year        = 2014:2025,
#'   group       = or(c("first_name", "last_name")))
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
not_year <- function(x) {
  params(x)[names2(params(x)) %!=% "year"]
}

#' @autoglobal
#' @noRd
map_match_query <- function(obj, qry) {

  param  <- not_year(qry)
  pname  <- rlang::names2(param)

  # TODO Use a modifier on "year"
  # parameter if meant for an API field?
  field <- keys(obj)
  clean <- purrr::map(field, clean_names)

  fin <- purrr::pmap(
    list(clean, field),
    function(cl, fl) {
      rlang::set_names(cl, fl)[c_match(pname, rlang::set_names(cl, fl))]
    }) |>
    purrr::compact()

  imap(fin, \(x, i) {
    param[names(param) %in% unlist(x, use.names = FALSE)]
  }) |>
      purrr::map2(fin, \(x, y) set_names(x, names2(y)))
}

#' @autoglobal
#' @noRd
match_query <- function(obj, qry) {

  param  <- not_year(qry)
  pname  <- names2(param)

  # TODO Use a modifier on "year"
  # parameter if meant for an API field?
  field <- keys(obj) |>
    unlist(use.names = FALSE) |>
    collapse::funique()

  clean <- clean_names(field)

  set_names(
    param[c_match(clean, pname)],
    field[sort(c_match(pname, clean))])
}

#' @autoglobal
#' @noRd
match_query2 <- function(obj, qry) {

  param <- not_year(qry)

  k <- keys(obj) |>
    unlist(use.names = FALSE) |>
    collapse::funique()

  vec <- set_names(k, clean_names(k))

  raw <- vec[names2(vec) %in_% names2(param)]

  values <- cheapr::cheapr_rep_each(
    param,
    cheapr::counts(
      names(raw))$count)

  named_args <- set_names(values, unname(raw))

  list(
    value = named_args,
    year = map(keys(obj), \(x) x[c_match(unname(raw), x)]))
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
