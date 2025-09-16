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
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Named conditions
#'  to filter results by. See Details.
#' @param call The environment from which the function is called.
#' @returns S7 `<class_query>` object.
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
#' try(query3()) # cannot be empty
#' try(query3(1)) # must be named
#' try(query3(a = 1, a = 2)) # names must be unique
#' try(query3(a = 1, or("a"))) # group needs more than one member
#' try(query3(a = 1, or("a", "year"))) # year cannot be grouped
#' try(query3(year = any_of(2000:2020))) # year cannot be modified
#'
#' query3(
#'   first_name  = starts_with("And"),
#'   middle_name = NULL,
#'   last_name   = contains("J"),
#'   state       = any_of("CA", "GA", "NY"),
#'   state_own   = c("GA", "MD"),
#'   ccn         = "01256",
#'   rate        = between(0.45, 0.67),
#'   year        = 2014:2025)
#'
#' query3(
#'   year  = 2018:2025,
#'   state = any_of("CA", "GA", "NY"),
#'   state_owner = c("GA", "MD"),
#'   or("state", "state_owner"))
#'
#' try(query3(
#'   first_name  = starts_with("And"),
#'   ccn         = "01256",
#'   and("ccn", "npii")))
#'
#' query3(
#'   year = 2022:2024,
#'   state = any_of("GA", "NY"),
#'   enrlmt_id = "I20040309000221",
#'   city = "Atlanta",
#'   provcity = "Atlanta",
#'   provider_name = starts_with("C"),
#'   provname = starts_with("C"),
#'   provider_first_name = starts_with("An"),
#'   provider_last_name = contains("JE"),
#'   practice_state_or_us_territory = any_of("GA", "FL"),
#'   practice_size = less_than(10, or_equal = TRUE),
#'   or("state", "city"),
#'   or("provider_name", "provider_first_name"))
#' @autoglobal
#' @export
query3 <- function(..., call = rlang::caller_env()) {

  check_query_dots(..., call = call)

  x <- rlang::enexprs(...) |>
    purrr::compact()

  x <- list(
    grps = purrr::keep(x, is_junc),
    mods = purrr::keep(x, is_mod),
    bare = purrr::keep(x, is_bare))

  check_all_named(c(x$mods, x$bare), call = call)
  check_names_unique(c(x$mods, x$bare), call = call)
  check_mods_year(x$mods, call = call)

  x$params <- eval_params(x$mods, x$bare)

  if (!empty(x$grps)) {

    check_group_members(x, call = call)

    x$grps  <- eval_groups(x$grps)
    grp_idx <- group_index(x$grps, x$params)

    member_of <- purrr::imap(
      grp_idx, function(idx, nm) map_members(x$params, idx, nm)) |>
      purrr::list_flatten(name_spec = "{inner}")

    x$params[unlist(grp_idx, use.names = FALSE)] <- member_of
  }

  # TODO Handle year differently

  if (any(rlang::names2(x$params) %in% "year")) {

    yr <- rlang::names2(x$params) %iin% "year"

    return(
      class_query(
        params = x$params[-yr],
        year   = x$params[yr]$year@value,
        groups = x$grps)
    )
  }

  class_query(
    params = x$params,
    groups = x$grps)
}
