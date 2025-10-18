#' @include aka_endpoints.R
#' @include aka_collections.R
NULL

#' @autoglobal
#' @noRd
check_alias_unique <- function(x) {
  if (collapse::any_duplicated(x)) {
    cli::cli_abort(
      c("x" = "Duplicate names found:",
        cli::col_yellow(
          cli::format_bullets_raw(
            paste(
              cli::symbol$arrow_right, " ",
              x[collapse::fduplicated(x)]
              )
            )
          )
        )
      )
  }
}

#' @autoglobal
#' @noRd
match_alias <- function(x) {
  collapse::fmatch(x, the$aka$nms)
}

#' @autoglobal
#' @noRd
is_alias <- function(x) {
  !cheapr::is_na(match_alias(x))
}

#' @autoglobal
#' @noRd
alias_match_endpoint <- function(x) {
  the$aka$all[match_alias(x)] |>
    unlist(use.names = FALSE)
}

#' @autoglobal
#' @noRd
alias_is_care <- function(x) {
  !cheapr::is_na(match_names(x, the$aka$clg$care))
}

#' @autoglobal
#' @noRd
alias_is_caid <- function(x) {
  !cheapr::is_na(match_names(x, the$aka$clg$caid))
}

#' @autoglobal
#' @noRd
alias_is_prov <- function(x) {
  !cheapr::is_na(match_names(x, the$aka$clg$prov))
}

#' @autoglobal
#' @noRd
alias_is_open <- function(x) {
  !cheapr::is_na(match_names(x, the$aka$clg$open))
}

#' @autoglobal
#' @noRd
alias_is_hgov <- function(x) {
  !cheapr::is_na(match_names(x, the$aka$clg$hgov))
}

#' @autoglobal
#' @noRd
alias_is_current <- function(x) {
  !cheapr::is_na(match_names(x, the$aka$pnt$current))
}

#' @autoglobal
#' @noRd
alias_is_temporal <- function(x) {
  !cheapr::is_na(match_names(x, the$aka$pnt$temporal))
}

#' @autoglobal
#' @noRd
alias_catalog_name <- function(x, call = rlang::caller_env()) {
  res <- kit::nif(
    alias_is_care(x), "care",
    alias_is_caid(x), "caid",
    alias_is_prov(x), "prov",
    alias_is_open(x), "open",
    alias_is_hgov(x), "hgov")

  res %|% cli::cli_abort(c("x" = "{.val {x}} does not belong to a catalog."), call = call)
}

#' @autoglobal
#' @noRd
alias_endpoint_type <- function(x, call = rlang::caller_env()) {
  res <- kit::nif(alias_is_current(x), "current", alias_is_temporal(x), "temporal")
  res %|% cli::cli_abort(c("x" = "{.val {x}} is not an endpoint."), call = call)
}

#' @autoglobal
#' @noRd
match_collection <- function(x) {
  collapse::fmatch(x, the$col$nms)
}

#' @autoglobal
#' @noRd
alias_is_collection <- function(x) {
  !cheapr::is_na(match_collection(x))
}

#' @autoglobal
#' @noRd
any_collection <- function(x) {
  any(alias_is_collection(x), na.rm = TRUE)
}

#' @autoglobal
#' @noRd
alias_match_collection <- function(x, call = rlang::caller_env()) {

  if (!alias_is_collection(x)) {
    cli::cli_abort(c("x" = "{.val {x}} is not a collection."), call = call)
  }

  the$col$all[match_collection(x)] |>
    unname() |>
    yank()
}
