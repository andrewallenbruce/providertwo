#' @include aka_end.R
#' @include aka_col.R
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
match_names <- function(x, aka) {
  collapse::fmatch(x, table = rlang::names2(aka))
}

#' @autoglobal
#' @noRd
rex_point <- function(x) {
  the$aka$all[match_alias(x)] |>
    unlist(use.names = FALSE)
}

#' @autoglobal
#' @noRd
is_alias <- function(x) {
  !cheapr::is_na(match_alias(x))
}

#' @autoglobal
#' @noRd
is_care_alias <- function(x) {
  !cheapr::is_na(match_names(x, the$aka$clg$care))
}

#' @autoglobal
#' @noRd
is_caid_alias <- function(x) {
  !cheapr::is_na(match_names(x, the$aka$clg$caid))
}

#' @autoglobal
#' @noRd
is_prov_alias <- function(x) {
  !cheapr::is_na(match_names(x, the$aka$clg$prov))
}

#' @autoglobal
#' @noRd
is_open_alias <- function(x) {
  !cheapr::is_na(match_names(x, the$aka$clg$open))
}

#' @autoglobal
#' @noRd
is_hgov_alias <- function(x) {
  !cheapr::is_na(match_names(x, the$aka$clg$hgov))
}

#' @autoglobal
#' @noRd
is_current_alias <- function(x) {
  !cheapr::is_na(match_names(x, the$aka$pnt$current))
}

#' @autoglobal
#' @noRd
is_temporal_alias <- function(x) {
  !cheapr::is_na(match_names(x, the$aka$pnt$temporal))
}

#' @autoglobal
#' @noRd
catalog_type <- function(x, call = rlang::caller_env()) {
  res <- kit::nif(
    is_care_alias(x), "care",
    is_caid_alias(x), "caid",
    is_prov_alias(x), "prov",
    is_open_alias(x), "open",
    is_hgov_alias(x), "hgov")

  res %|% cli::cli_abort(c("x" = "{.val {x}} does not belong to a catalog."), call = call)
}

#' @autoglobal
#' @noRd
point_type <- function(x, call = rlang::caller_env()) {
  res <- kit::nif(is_current_alias(x), "current", is_temporal_alias(x), "temporal")
  res %|% cli::cli_abort(c("x" = "{.val {x}} is not an endpoint."), call = call)
}
