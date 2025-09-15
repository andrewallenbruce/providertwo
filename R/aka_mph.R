#' @include aka_end.R
#' @include aka_col.R
NULL

#' @autoglobal
#' @noRd
init <- function(...) {
  x <- rlang::names2(c(...))

  if (collapse::any_duplicated(x)) {
    dupes <- x[collapse::fduplicated(x)]

    cli::cli_abort(
      c("x" = "Duplicate names found:",
        cli::col_yellow(cli::format_bullets_raw(
          paste(cli::symbol$arrow_right, " ", dupes)
    ))))
  }
  x
}

#' @autoglobal
#' @noRd
match_point <- function(x) {
  collapse::fmatch(x, aka$nms)
}

#' @autoglobal
#' @noRd
rex_point <- function(x) {
  aka$all[match_point(x)] |>
    unlist(use.names = FALSE)
}

#' @autoglobal
#' @noRd
is_care_member <- function(x) {
  !cheapr::is_na(collapse::fmatch(x, aka$clg$care))
}

#' @autoglobal
#' @noRd
is_caid_member <- function(x) {
  !cheapr::is_na(collapse::fmatch(x, aka$clg$caid))
}

#' @autoglobal
#' @noRd
is_prov_member <- function(x) {
  !cheapr::is_na(collapse::fmatch(x, aka$clg$prov))
}

#' @autoglobal
#' @noRd
is_open_member <- function(x) {
  !cheapr::is_na(collapse::fmatch(x, aka$clg$open))
}

#' @autoglobal
#' @noRd
is_hgov_member <- function(x) {
  !cheapr::is_na(collapse::fmatch(x, aka$clg$hgov))
}

#' @autoglobal
#' @noRd
is_current <- function(x) {
  !cheapr::is_na(collapse::fmatch(x, aka$pnt$current))
}

#' @autoglobal
#' @noRd
is_temporal <- function(x) {
  !cheapr::is_na(collapse::fmatch(x, aka$pnt$temporal))
}

#' @autoglobal
#' @noRd
catalog_type <- function(x, call = rlang::caller_env()) {
  res <- kit::nif(
    is_care_member(x), "care",
    is_caid_member(x), "caid",
    is_prov_member(x), "prov",
    is_open_member(x), "open",
    is_hgov_member(x), "hgov")

  res %|% cli::cli_abort(c("x" = "{.val {x}} does not belong to a catalog."), call = call)
}

#' @autoglobal
#' @noRd
point_type <- function(x, call = rlang::caller_env()) {
  res <- kit::nif(is_current(x), "current", is_temporal(x), "temporal")
  res %|% cli::cli_abort(c("x" = "{.val {x}} is not an endpoint."), call = call)
}
