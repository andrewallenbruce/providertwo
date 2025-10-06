#' @noRd
#' @autoglobal
class_query <- S7::new_class(
  name       = "class_query",
  package    = NULL,
  properties = list(
    params   = S7::class_list,
    year     = S7::class_integer,
    groups   = S7::class_list
  )
)

#' Create a Query Object
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Key-value pairs of query parameters.
#'   See Details.
#'
#' @param call The environment from which the function is called.
#'
#' @returns S7 `<class_query>` object.
#'
#' @name query
NULL

#' @rdname query
#' @examples
#' query(
#'   first_name  = starts_with("And"),
#'   middle_name = NULL,
#'   last_name   = contains("J"),
#'   state       = any_of("CA", "GA", "NY"),
#'   state_own   = c("GA", "MD"),
#'   ccn         = "01256",
#'   rate        = between(0.45, 0.67),
#'   year        = 2014:2025)
#' @autoglobal
#' @export
query <- function(...) {
  class_query(
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
#' try(query2())
#' try(query2(1))
#' try(query2(a = 1, a = 2))
#' try(query2(a = 1, or("a")))
#' try(query2(a = 1, or("a", "year")))
#' try(query2(year = none_of(2000:2020)))
#' try(query2(ccn = "01256", and("ccn", "npii")))
#'
#' query2(
#'   first_name  = starts_with("And"),
#'   middle_name = NULL,
#'   last_name   = contains("J"),
#'   state       = c("CA", "GA", "NY"),
#'   state_own   = c("GA", "MD"),
#'   ccn         = "01256",
#'   rate        = between(0.45, 0.67),
#'   year        = 2014:2025)
#'
#' query2(
#'   year  = 2018:2025,
#'   state = c("CA", "GA", "NY"),
#'   state_owner = c("GA", "MD"),
#'   or("state", "state_owner"))
#'
#' query2(
#'   year = 2022:2024,
#'   state = c("GA", "NY"),
#'   enrlmt_id = "I20040309000221",
#'   city = "Atlanta",
#'   provcity = "Atlanta",
#'   provider_name = starts_with("C"),
#'   provname = starts_with("C"),
#'   provider_first_name = starts_with("An"),
#'   provider_last_name = contains("JE"),
#'   practice_state_or_us_territory = c("GA", "FL"),
#'   practice_size = less_than(10, or_equal = TRUE),
#'   or("state", "city"),
#'   or("provider_name", "provider_first_name"))
#' @autoglobal
#' @export
query2 <- function(..., call = rlang::caller_env()) {

  check_query_dots(..., call = call)

  x <- purrr::compact(
    rlang::enexprs(
      ...,
      # .homonyms = "error",
      .ignore_null = "all"
      )
    )

  x <- rlang::list2(
    grps = purrr::keep(x, is_junc),
    mods = is_year(purrr::keep(x, is_mod), negate = TRUE),
    bare = is_year(purrr::keep(x, is_bare), negate = TRUE),
    !!!is_year(x))

  check_query_params(x, call = call)

  x$params <- eval_params(x$mods, x$bare)

  if (!empty(x$grps)) {

    check_query_members(x, call = call)

    x$grps <- eval_groups(x$grps)
    x$idx  <- group_index(x$grps, x$params)

    member_of <- purrr::imap(x$idx, function(idx, nm)
      map_members(x$params, idx, nm)) |>
      purrr::list_flatten(name_spec = "{inner}")

    x$params[unlist(x$idx, use.names = FALSE)] <- member_of
  }

  if (empty(x$year)) {

    class_query(params = x$params, groups = x$grps)

   } else {

    class_query(
      params = x$params,
      year = eval(x$year),
      groups = x$grps
    )
  }
}
