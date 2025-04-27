#' @autoglobal
#' @noRd
pro_main <- function(x, call = caller_env()) {
  x <- switch(
    x,
    affiliations    = "^Facility Affiliation Data$",
    clinicians      = "^National Downloadable File$",
    utilization     = "^Utilization Data$",
    MIPS_perform    = "^PY 2022 Clinician Public Reporting: Overall MIPS Performance$",
    MIPS_patient    = "^PY 2022 Group Public Reporting: Patient Experience$",
    MIPS_clinician  = "^PY 2022 Clinician Public Reporting: MIPS Measures and Attestations$",
    MIPS_group      = "^PY 2022 Group Public Reporting: MIPS Measures and Attestations$",
    MIPS_virtual    = "^PY 2022 Virtual Group Public Reporting: MIPS Measures and Attestations$",
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )

  if (!exists("catalog")) .catalog <- catalogs()

  res <- select_alias(.catalog$pro, x)

  if (empty(res)) cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)

  c(res)

}

#' @autoglobal
#' @noRd
pro_group <- function(x, call = caller_env()) {
  switch(
    x,
    MIPS = list(
      group = "PY 2022 MIPS Public Reporting",
      alias = c("MIPS_perform",
                "MIPS_patient",
                "MIPS_clinician",
                "MIPS_group",
                "MIPS_virtual"
                )
      ),
    PDC = list(
      group = "Provider Data Catalog",
      alias = c("affiliations",
                "clinicians",
                "utilization"
                )
      ),
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
}
