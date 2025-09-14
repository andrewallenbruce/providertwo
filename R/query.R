#' @noRd
#' @autoglobal
class_query <- S7::new_class(
  name       = "class_query",
  package    = NULL,
  properties = list(
    params   = S7::class_list,
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

#' @autoglobal
#' @noRd
check_names_unique <- function(x, call = rlang::caller_env()) {

  if (collapse::any_duplicated(rlang::names2(x[rlang::have_name(x)]))) {

    n <- rlang::names2(x[rlang::have_name(x)])
    n <- n[collapse::fduplicated(n)]

    cli::cli_abort(
      c("x" = "{.field Query} names must be unique",
        "!" = "Field{?s} {.field {n}} appea{?rs/rs/r} multiple times."),
      call  = call)
  }
}

#' @autoglobal
#' @noRd
check_bare_unnamed <- function(x, call = rlang::caller_env()) {

  if (!all(rlang::have_name(x))) {

    idx <- !rlang::have_name(x)

    cli::cli_abort(
      c("x" = "{.field Query} values must be named with a {.field field}.",
        "!" = "Unnamed value{?s}: {.field {x[idx]}}."),
      call  = call)
  }
}

#' @autoglobal
#' @noRd
check_group_length <- function(x, g, call = rlang::caller_env()) {
  # check no empty/single-member groups
  if (any(cheapr::list_lengths(g) < 2L)) {

    bad <- x[cheapr::list_lengths(g) < 2L] |>
      purrr::map(deparse1) |>
      purrr::list_c()

    cli::cli_abort(
      c("x" = "{.field Query} groups must have 2 or more {.field members}.",
        "!" = "Group{?s} {.field {bad}} ha{?s/ve/s} less than 2 members."),
      call  = call)
  }
}

#' @autoglobal
#' @noRd
check_memb_dupes <- function(x, call = rlang::caller_env()) {
  # check no duplicate group members
  if (collapse::any_duplicated(x)) {

    cli::cli_abort(
      c("x" = "{.field Query} group members must be unique",
        "!" = "Member{?s} {.field {x[collapse::fduplicated(x)]}} appea{?rs/r} in multiple groups."),
      call  = call)
  }
}

#' @autoglobal
#' @noRd
check_membs_are_params <- function(x, p, call = rlang::caller_env()) {
  # check group members are param names
  if (!all(x %in% rlang::names2(p))) {

    bad <- x[!x %in% rlang::names2(p)]

    cli::cli_abort(
      c("x" = "{.field Query} group members must be {.field query} field names",
        "!" = "Member{?s} {.field {bad}} d{?oes/o} not match any {.field query} field names."),
      call  = call)
  }
}

#' @autoglobal
#' @noRd
check_group_members <- function(x, call = rlang::caller_env()) {
  # TODO
  # check no duplicate groups
  # check no nested groups

  g <- purrr::map(x$grps, \(x) as.character(x)[-1])

  check_group_length(x, g, call = call)

  m <- purrr::list_c(g)

  check_memb_dupes(m, call = call)

  check_membs_are_params(m, c(x$bare, x$mods), call = call)
}

#' @rdname query
#' @examples
#' query3(
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

  check_names_unique(c(x$mods, x$bare))
  check_bare_unnamed(x$bare)
  check_group_members(x)

  groups  <- eval_groups(x$grps)
  params  <- eval_params(x$mods, x$bare)
  members <- get_members(groups)

  idx <- purrr::map(members, \(y) rlang::names2(params) %iin% y)

  params[unlist(idx, use.names = FALSE)] <- purrr::imap(
    idx, function(g, i) purrr::map2(params[g], i, set_members)) |>
    purrr::list_flatten(name_spec = "{inner}")

  class_query(params = params, groups = groups)
}
