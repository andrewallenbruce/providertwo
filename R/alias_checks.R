# x <- list(
#   alias = "example",
#   regex = "^example$",
#   tbl = fastplyr::new_tbl())
# check_alias_results(x)
#' @autoglobal
#' @noRd
check_alias_results <- function(alias, df, call = rlang::caller_env()) {
  msg <- c("x" = "{.field {alias}} had {.strong {nrow(df)}} matches.")

  if (nrow(df) == 0L) {
    cli::cli_abort(msg, call = call)
  }

  if (nrow(df) > 1L) {
    msg <- c(msg, cli::col_yellow(cli::format_bullets_raw(df$title)))
    cli::cli_abort(msg, call = call)
  }

}

#' @autoglobal
#' @noRd
check_endpoint_alias <- function(alias, call = rlang::caller_env()) {
  if (length(alias) > 1L) {
    cli::cli_abort(c("x" = "Only one {.cls endpoint} can be loaded at a time."),
                   call = call)
  }

  if (!is_alias(alias)) {
    msg <- c("x" = "{.val {alias}} is not an {.cls endpoint} alias.")

    if (alias_is_collection(alias)) {
      cli::cli_abort(c(msg, "!" = "{.val {alias}} is a {.cls collection} alias."),
                     call = call)
    }
    cli::cli_abort(c(msg), call = call)
  }
}


#' @autoglobal
#' @noRd
check_collection_alias <- function(alias, call = rlang::caller_env()) {
  if (length(alias) > 1L) {
    cli::cli_abort(
      c("x" = "Only one {.cls collection} can be loaded at a time."),
      call = call)
  }

  if (!alias_is_collection(alias)) {
    cli::cli_abort(
      c("x" = "{.val {alias}} is not a {.cls collection} alias."),
      call = call)
  }
}

#' @autoglobal
#' @noRd
check_group_alias <- function(alias, call = rlang::caller_env()) {

  avec  <- unlist(alias, use.names = FALSE)
  msg   <- c("x" = "A {.cls group} must contain more than one {.cls endpoint}.")

  if (rlang::is_empty(alias) || !any(nzchar(alias))) {
    cli::cli_abort(msg, call = call)
  }

  if (any_collection(avec)) {
    avec <- avec[alias_is_collection(avec)][[1]]
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
