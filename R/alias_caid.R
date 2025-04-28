#' @autoglobal
#' @noRd
select_caid_main <- function(x, call = caller_env()) {
  x <- switch(
    x,
    MLR            = "MLR Summary Reports",
    enterprise     = "Medicaid Enterprise System Datatable",

    DEMO_well      = "Medicaid and CHIP enrollees who received a well-child visit",
    DEMO_sud       = "Medicaid and CHIP enrollees who received mental health or SUD services",
    DEMO_disable   = "Medicaid enrollees who qualify for benefits based on disability",
    DEMO_premature = "^Prematurity and severe maternal morbidity among Medicaid",
    DEMO_language  = "Primary language spoken by the Medicaid and CHIP population",
    DEMO_race      = "Race and ethnicity of the national Medicaid and CHIP population",
    DEMO_rural     = "Rural Medicaid and CHIP enrollees",
    DEMO_waive     = "Section 1915",

    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )

  if (!exists("catalog")) .catalog <- catalogs()

  res <- select_alias(.catalog$caid$main, x)

  if (empty(res)) cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)

  c(res)

}

#' @autoglobal
#' @noRd
select_caid_group <- function(x, call = caller_env()) {
  switch(
    x,
    demographics = list(
      group = "Medicaid and CHIP Enrollee Demographics",
      alias = c(
        "DEMO_well",
        "DEMO_sud",
        "DEMO_disable",
        "DEMO_premature",
        "DEMO_language",
        "DEMO_race",
        "DEMO_rural",
        "DEMO_waive"
        )
      ),
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
}

#' @autoglobal
#' @noRd
select_caid_temp <- function(x, call = caller_env()) {

  x <- switch(
    x,
    NADAC = "NADAC",
    MCP = "Managed Care Programs By State",
    rebate = "^Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program",
    blood = "^Pricing Comparison for Blood Disorder Treatments",
    drug = "^State Drug Utilization Data",
    HCQ = "Child and Adult Health Care Quality Measures",
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call))

  if (!exists("catalog")) .catalog <- catalogs()

  res <- select_alias(.catalog$caid$temp, x)

  if (empty(res)) cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)

  list_tidy(
    !!!c(slt(res, -data)),
    endpoints   = get_elem(res, "data") |> _[[1]],
    identifier  = endpoints$identifier[1]
  )

}
