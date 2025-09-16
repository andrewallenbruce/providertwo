# x <- list(
#   alias = "example",
#   regex = "^example$",
#   tbl = fastplyr::new_tbl())
# check_alias_results(x)
#' @autoglobal
#' @noRd
check_alias_results <- function(x, call = rlang::caller_env()) {
  msg <- c("x" = "{.field {x$alias}} had {.strong {nrow(x$tbl)}} matches.")

  if (nrow(x$tbl) == 0L) {
    cli::cli_abort(msg, call = call)
  }

  if (nrow(x$tbl) > 1L) {
    msg <- c(msg, cli::col_yellow(cli::format_bullets_raw(x$tbl$title)))
    cli::cli_abort(msg, call = call)
  }

}

#' @autoglobal
#' @noRd
check_collection <- function(alias, call = rlang::caller_env()) {
  if (length(alias) > 1L) {
    cli::cli_abort(
      c("x" = "Only one {.cls collection} can be loaded at a time."),
      call = call)
  }

  if (!is_collect(alias)) {
    cli::cli_abort(
      c("x" = "{.val {alias}} is not a {.cls collection}."),
      call = call)
  }
}

#' @autoglobal
#' @noRd
check_group <- function(alias, call = rlang::caller_env()) {

  avec  <- unlist(alias, use.names = FALSE)
  msg   <- c("x" = "A {.cls group} must contain at least two {.cls endpoints}.")

  if (rlang::is_empty(alias) || !any(nzchar(alias))) {
    cli::cli_abort(msg, call = call)
  }

  if (any_collect(avec)) {
    avec <- avec[is_collect(avec)][[1]]
    cli::cli_abort(
      c("x" = "A {.cls group} cannot contain a {.cls collection}.",
        ">" = "Run e.g., {.field collection({.val {avec}})}."),
      call = call
    )
  }

  if (rlang::has_length(avec, 1L)) {
    cli::cli_abort(
      c(msg, ">" = "Run {.field endpoint({.val {avec}})} to load this endpoint."),
      call = call)
  }
}

#' @autoglobal
#' @noRd
check_year <- function(x,
                       min = NULL,
                       arg = rlang::caller_arg(x),
                       call = rlang::caller_env()) {
  check_number_whole(
    x,
    min        = min,
    max        = this_year(),
    allow_na   = FALSE,
    allow_null = FALSE,
    arg        = arg,
    call       = call
  )

  if (ceiling(log10(x)) != 4L) {
    cli::cli_abort("{.arg {arg}} must be 4 digits",
                   arg  = arg,
                   call = call)
  }
}

#' @autoglobal
#' @noRd
check_years <- function(x,
                        min = NULL,
                        arg = rlang::caller_arg(x),
                        call = rlang::caller_env()) {
  purrr::walk(x, function(x)
    check_year(
      x,
      min = min,
      arg = arg,
      call = call
    ))
}

#' @autoglobal
#' @noRd
check_requires <- function(x) {
  purrr::walk(x, check_required)
}

#' @autoglobal
#' @noRd
check_10_digits <- function(x, call = rlang::caller_env()) {
  if (ceiling(log10(x)) != 10L) {
    cli::cli_abort(c("x" = "{.arg x} must have 10 digits"), call = call)
  }
}

#' @autoglobal
#' @noRd
luhn_impl <- function(x) {
  check <- x %% 10L
  x <- cheapr::cheapr_rev(as.integer(unlist(strsplit(
    as.character(as.integer(x %/% 10L)), ""
  ))))
  x[c(1L, 3L, 5L, 7L, 9L)]  <- x[c(1L, 3L, 5L, 7L, 9L)] * 2L
  x[cheapr::which_(x > 9L)] <- x[cheapr::which_(x > 9L)] - 9L
  x <- z <- collapse::fsum(x) + 24
  x <- ceiling(x / 10L) * 10L - z
  x == check
}

#' @autoglobal
#' @noRd
luhn_check <- function(x) {
  if (rlang::has_length(x, 1L)) {
    return(luhn_impl(x))
  }
  purrr::map_lgl(x, luhn_impl)
}

#' @autoglobal
#' @noRd
assert_luhn <- function(x, call = rlang::caller_env()) {
  if (!any(luhn_check(x))) {
    idx <- cheapr::which_(luhn_check, invert = TRUE)
    cli::cli_abort(
      c("x" = "{.val {length(idx)}} {.field npi{?s}} failed Luhn algorithm check:",
        cli::col_yellow(
          cli::format_bullets_raw(
            paste(cli::symbol$arrow_right, just_right(brackets(idx)), x[idx])
      ))),
      call = call
    )
  }
}

#' @autoglobal
#' @noRd
assert_nchars <- function(x, n, xname, call = rlang::caller_env()) {
  if (any(nchar(x) != n)) {
    invalid <- x[which_(nchar(x) != n)]
    cli::cli_abort(
      c("Invalid {.arg {xname}} entered: {.val {invalid}}",
        ">" = "{.arg {xname}} must be {n} characters long"),
      call = call
    )
  }
}

#' @autoglobal
#' @noRd
assert_enum <- function(x, choices, xname, call = rlang::caller_env()) {
  if (any(!x %in% choices)) {
    invalid <- x[which_(x %in% choices, invert = TRUE)]
    cli::cli_abort(c("Invalid {.arg {xname}} entered: {.val {invalid}}"), call = call)
  }
}

# check_params_fields <- function(params, fields) {
#   if (!any(params %in% fields)) {
#     invalid_params <- setdiff(params, fields)
#     abort(glue("Invalid Parameter(s): {invalid_params}"))
#   }
# }
