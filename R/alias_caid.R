#' @autoglobal
#' @noRd
caid_main <- function(x, call = caller_env()) {
  x <- switch(
    x,
    # pi         = "^PI dataset$", # NOT WORKING
    # blood      = "Pricing Comparison for Blood Disorder Treatments //(Pricing as of 12/1/2024//)", # GROUP

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
caid_group <- function(x, call = caller_env()) {
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
caid_temp <- function(x, call = caller_env()) {

  x <- switch(
    x,
    newdrug_01 = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program 01-06-2025-to-01-12-2025",
    newdrug_02 = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program 01-13-2025-to-01-19-2025",
    newdrug_03 = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program 01-20-2025-to-01-26-2025",
    newdrug_04 = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program 01-27-2025-to-02-02-2025",
    newdrug_05 = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program 02-03-2025-to-02-09-2025",
    newdrug_06 = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program 02-10-2025-to-02-16-2025",
    newdrug_07 = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program 02-17-2025-to-02-23-2025",
    newdrug_08 = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program 02-24-2025-to-03-02-2025",
    newdrug_09 = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program 03-03-2025-to-03-09-2025",
    newdrug_10 = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program 03-10-2025-to-03-16-2025",
    newdrug_11 = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program 03-17-2025-to-03-23-2025",
    newdrug_12 = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program 03-24-2025-to-03-30-2025",
    newdrug_13 = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program 03-31-2025-to-04-06-2025",
    newdrug_14 = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program 04-07-2025-to-04-13-2025",
    newdrug_15 = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program 04-14-2025-to-04-20-2025",
    newdrug_16 = "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program 12-30-2024-to-01-05-2025",
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
