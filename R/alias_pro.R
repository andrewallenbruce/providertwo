#' @autoglobal
#' @noRd
pro_main <- function(x, call = caller_env()) {
  x <- switch(
    x,
    affiliations            = "^Facility Affiliation Data$",
    clinicians              = "^National Downloadable File$",
    utilization             = "^Utilization Data$",
    MIPS_performance        = "^PY 2022 Clinician Public Reporting: Overall MIPS Performance$",
    MIPS_patient            = "^PY 2022 Group Public Reporting: Patient Experience$",
    MIPS_measures_clinician = "^PY 2022 Clinician Public Reporting: MIPS Measures and Attestations$",
    MIPS_measures_group     = "^PY 2022 Group Public Reporting: MIPS Measures and Attestations$",
    MIPS_measures_virtual   = "^PY 2022 Virtual Group Public Reporting: MIPS Measures and Attestations$",
    cli_abort(c("x"         = "No matches found for {.val {x}}."), call = call)
  )

  if (!exists("catalog")) .catalog <- catalogs()

  select_alias(.catalog$pro, x) |> c()

}

#' @autoglobal
#' @noRd
pro_group <- function(x) {
  switch(
    x,
    MIPS = list(
      group = "PY 2022 MIPS Public Reporting",
      alias = c("MIPS_performance",
                "MIPS_patient",
                "MIPS_measures_clinician",
                "MIPS_measures_group",
                "MIPS_measures_virtual")),
    PDC = list(
      group = "Provider Data Catalog",
      alias = c("affiliations",
                "clinicians",
                "utilization")),
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
}
