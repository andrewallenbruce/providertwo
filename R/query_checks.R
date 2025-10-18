#' @autoglobal
#' @noRd
check_class_query <- function(x, call = rlang::caller_env()) {
  if (!S7::S7_inherits(x, class_query)) {
    cli::cli_abort(c(
      "x" = paste0(
        "{.field qry} must be of {.cls class_query}, ",
        "not {.obj_type_friendly {x}}."
      )
    ), call = call)
  }
}

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
