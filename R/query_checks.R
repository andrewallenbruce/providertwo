#' @autoglobal
#' @noRd
check_names_unique <- function(x, call = rlang::caller_env()) {

  if (collapse::any_duplicated(rlang::names2(x[rlang::have_name(x)]))) {

    n <- rlang::names2(x[rlang::have_name(x)])
    n <- n[collapse::fduplicated(n)]

    cli::cli_abort(
      c("x" = "{.field Query} names must be {.strong unique}",
        "!" = "Field{?s} {.field {n}} appea{?rs/rs/r} multiple times."),
      call  = call)
  }
}

#' @autoglobal
#' @noRd
check_all_named <- function(x, call = rlang::caller_env()) {

  if (!rlang::is_named(x)) {

    idx <- !rlang::have_name(x)

    cli::cli_abort(
      c("x" = "{.field Query} values must be {.strong named}.",
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
      c("x" = "Query {.field groups} must have {.emph 2 or more} {.field members}.",
        "!" = "Group{?s} {.field {bad}} ha{?s/ve} less than 2 members."),
      call  = call)
  }
}

#' @autoglobal
#' @noRd
check_memb_dupes <- function(x, call = rlang::caller_env()) {
  # check no duplicate group members
  if (collapse::any_duplicated(x)) {

    i <- collapse::fduplicated(x)

    cli::cli_abort(
      c("x" = "Query {.field group members} must be {.emph unique}.",
        "!" = "Member{?s} {.field {x[i]}} appea{?rs/r} in multiple groups."),
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
      c("x" = "Query {.field group members} must be query {.field field names}",
        "!" = "Member{?s} {.field {bad}} d{?oes/o} not match any {.field fields}"),
      call  = call)
  }
}

#' @autoglobal
#' @noRd
check_memb_no_year <- function(x, call = rlang::caller_env()) {
  # check no group member is "year"
  if ("year" %in% x) {

    bad <- x[!x %in% rlang::names2(p)]

    cli::cli_abort(
      c("x" = "{.val year} cannot be a group member"),
      call  = call)
  }
}

#' @autoglobal
#' @noRd
check_group_members <- function(x, call = rlang::caller_env()) {
  # TODO
  # check no duplicate groups
  # check no nested groups

  g <- purrr::map(x$grps, function(x) as.character(x)[-1])

  check_group_length(x, g, call = call)

  m <- purrr::list_c(g)

  check_memb_no_year(m, call = call)

  check_memb_dupes(m, call = call)

  check_membs_are_params(m, c(x$bare, x$mods), call = call)
}
