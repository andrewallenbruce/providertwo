#' @autoglobal
#' @noRd
check_query_dots <- function(..., call = rlang::caller_env()) {
  if (...length() == 0L) {
    cli::cli_abort(c("x" = "{.field Query} cannot be {.strong empty}."), call = call)
  }
}

#' @autoglobal
#' @noRd
check_query_year <- function(x, call = rlang::caller_env()) {

  if (is_mod(x)) {
    cli::cli_abort(
      c("x" = "{.var year} cannot be used with a {.cls modifier}."),
      call  = call)
  }
}

#' @autoglobal
#' @noRd
check_query_names_unique <- function(x, call = rlang::caller_env()) {

  if (collapse::any_duplicated(rlang::names2(x[rlang::have_name(x)]))) {

    n <- rlang::names2(x[rlang::have_name(x)])
    n <- n[collapse::fduplicated(n)]

    cli::cli_abort(
      c("x" = "{.field Query} field names must be {.strong unique}",
        "!" = "Field{?s} {.field {n}} appea{?rs/rs/r} multiple times."),
      call  = call)
  }
}

#' @autoglobal
#' @noRd
check_query_named <- function(x, call = rlang::caller_env()) {

  if (!rlang::is_named(x) & any(are_not_empty(x))) {

    idx <- !rlang::have_name(x) & are_not_empty(x)

    cli::cli_abort(
      c("x" = "{.field Query} values must be {.strong named}.",
        "!" = "Unnamed value{?s}: {.val {x[idx]}}."),
      call  = call)
  }
}

#' @autoglobal
#' @noRd
check_query_params <- function(x, call = rlang::caller_env()) {
  check_query_named(c(x$mods, x$bare), call = call)
  check_query_names_unique(c(x$mods, x$bare), call = call)
  check_query_year(x$year, call = call)
}

#' @autoglobal
#' @noRd
check_query_group_lengths <- function(x, g, call = rlang::caller_env()) {
  # check no empty/single-member groups
  if (any(cheapr::list_lengths(g) < 2L)) {

    bad <- x[cheapr::list_lengths(g) < 2L] |>
      purrr::map(deparse1) |>
      purrr::list_c()

    cli::cli_abort(
      c("x" = "Query {.field groups} must have {.emph more than 1} {.field member}.",
        "!" = "Invalid group{?s}: {.field {bad}}."),
      call  = call)
  }
}

#' @autoglobal
#' @noRd
check_query_members_unique <- function(x, call = rlang::caller_env()) {
  # check no duplicate group members
  if (collapse::any_duplicated(x)) {

    i <- collapse::fduplicated(x)

    cli::cli_abort(
      c("x" = "Group {.field members} must be {.emph unique}.",
        "!" = "Member{?s} {.val {x[i]}} appea{?rs/r} multiple times."),
      call  = call)
  }
}

#' @autoglobal
#' @noRd
check_query_members_valid <- function(x, p, call = rlang::caller_env()) {
  # check group members are param names
  if (!all(x %in% rlang::names2(p))) {

    bad <- x[!x %in% rlang::names2(p)]

    cli::cli_abort(
      c("x" = "Group {.field members} must match a {.field field} name.",
        "!" = "Invalid member{?s}: {.val {bad}}."),
      call  = call)
  }
}

#' @autoglobal
#' @noRd
check_query_members_year <- function(x, call = rlang::caller_env()) {
  # check no group member is "year"
  if ("year" %in% x) {
    cli::cli_abort(c("x" = "{.val year} cannot be a group member."), call  = call)
  }
}


#' @autoglobal
#' @noRd
check_query_members <- function(x, call = rlang::caller_env()) {

  # TODO no duplicate groups
  # TODO no nested groups

  g <- purrr::map(x$grps, function(x) as.character(x)[-1])

  check_query_group_lengths(x$grps, g, call = call)

  m <- purrr::list_c(g)

  check_query_members_year(m, call = call)
  check_query_members_unique(m, call = call)
  check_query_members_valid(m, c(x$bare, x$mods), call = call)
}
