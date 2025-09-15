#' @autoglobal
#' @noRd
minit <- function(...) {

  x <- rlang::names2(c(...))

  if (collapse::any_duplicated(x)) {

    dupes <- x[collapse::fduplicated(x)]

    cli::cli_abort(c(
      "x" = "Duplicate names found:",
      cli::col_yellow(
        cli::format_bullets_raw(
          paste(cli::symbol$arrow_right, " ", dupes)
        ))
    ))
  }
  oomph::mph_init(x)
}
