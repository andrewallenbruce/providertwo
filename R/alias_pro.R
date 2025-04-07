#' @examplesIf rlang::is_interactive()
#' pro_main("affiliations")
#' pro_main("clinicians")
#' pro_main("utilization")
#' @autoglobal
#' @noRd
pro_main <- function(x) {

  x <- nswitch(
    x,
    "affiliations",  "^Facility Affiliation Data$",
    "clinicians",    "^National Downloadable File$",
    "utilization",   "^Utilization Data$",
    default = NA_character_,
    nThread = 4L
  )

  if (na(x)) cli_abort(c("x" = "No matches found."), call = call)

  if (!exists("catalog")) .catalog <- catalogs()

  select_alias(.catalog$pro, x) |> c()

}

#' @examplesIf rlang::is_interactive()
#' pro_group("mips")
#' @autoglobal
#' @noRd
pro_group <- function(x) {

  x <- nswitch(
    x,
    "mips", "^PY 2022",
    default = NA_character_,
    nThread = 4L)

  if (na(x)) cli_abort(c("x" = "No matches found."), call = call)

  if (!exists("catalog")) .catalog <- catalogs()

  select_alias(.catalog$pro, x)
}
