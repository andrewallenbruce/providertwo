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
#' @autoglobal
#' @export
query3 <- function(...) {

  x <- purrr::compact(rlang::enexprs(...))

  # TODO Handle no groups

  x <- list(
    grps = purrr::keep(x, is_junc),
    mods = purrr::keep(x, is_mod),
    bare = purrr::keep(x, is_bare))

  check_all_named(c(x$mods, x$bare))
  check_names_unique(c(x$mods, x$bare))
  check_group_members(x)

  groups  <- eval_groups(x$grps)
  params  <- eval_params(x$mods, x$bare)
  members <- get_members(groups)

  grp_idx <- purrr::map(members, function(x) rlang::names2(params) %iin% x)

  member_of <- purrr::imap(grp_idx, function(idx, nm)
    purrr::map2(params[idx], nm, set_members)) |>
    purrr::list_flatten(name_spec = "{inner}")

  params[unlist(grp_idx, use.names = FALSE)] <- member_of

  if (!rlang::is_empty(rlang::names2(params) %iin% "year")) {

    yr <- rlang::names2(params) %iin% "year"

    return(
      class_query(
        params = params[-yr],
        year   = params[yr]$year@value,
        groups = groups)
      )
  }

  class_query(
    params = params,
    groups = groups)
}
