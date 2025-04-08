# pro_main("affiliations")
# pro_main("clinicians")
# pro_main("utilization")
#' @autoglobal
#' @noRd
pro_main <- function(x, call = caller_env()) {
  x <- switch(
    x,
    affiliations = "^Facility Affiliation Data$",
    clinicians   = "^National Downloadable File$",
    utilization  = "^Utilization Data$",
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )

  if (!exists("catalog")) .catalog <- catalogs()

  select_alias(.catalog$pro, x) |> c()

}

# pro_group("mips")
#' @autoglobal
#' @noRd
pro_group <- function(x) {
  x <- switch(
    x,
    mips            = "^PY 2022",
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )

  if (!exists("catalog")) .catalog <- catalogs()

  select_alias(.catalog$pro, x)
}
