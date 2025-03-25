#' @include S7_backend.R
NULL

print <- S7::new_external_generic("base", "print", "x")


S7::method(print, MainCurrent) <- function(x) {
  CLI$title(x)
  cli::cat_line()
  CLI$desc(x)
  cli::cat_line()
  CLI$dim(x)
  cli::cat_line()

  cli::cli_bullets(
         c(
           "*" = "{CLI$txt_mod} {CLI$sym_pt} {CLI$mod(x)}",
           "*" = "{CLI$txt_span} {CLI$sym_pt} {CLI$temp(x)}",
           "*" = "{CLI$txt_per} {CLI$sym_pt} {CLI$period(x)}"
           )
         )
  cli::cat_line()

  if (rlang::is_interactive()) {
    cli::cli_bullets(
      c("i" = paste(
        CLI$link(x@site, "Site"),
        CLI$link(x@dictionary, "Dictionary"),
        CLI$link(x@references, "References"),
        sep = " | ")))
    }
  invisible(x)
}
