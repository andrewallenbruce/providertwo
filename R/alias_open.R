#' @autoglobal
#' @noRd
open_main <- function(x, call = caller_env()) {
  x <- switch(
    x,
    PROF_covered      = "^Covered Recipient Profile Supplement$",
    PROF_physician    = "^Physician \\(Distinct\\) Profile Information$",
    PROF_information  = "^Profile Information$",
    PROF_mapping      = "^Provider Profile ID Mapping Table$",
    PROF_entity       = "^Reporting Entity Profile Information$",
    PROF_teaching     = "^Teaching Hospital Profile Information$",
    SUMM_dashboard    = "^Summary Dashboard",
    SUMM_state_all    = "^State Level Payment Total and Averages for all Years$",
    SUMM_state_group  = "^State Payment Totals and Averages Grouped by Nature of Payment for all Years$",
    SUMM_nation_all   = "^National Level Payment Total and Averages for all Years$",
    SUMM_nation_group = "^National Level Payment Total and Averages by Provider Specialty for all Years$",
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )

  if (!exists("catalog")) .catalog <- catalogs()

  select_alias(.catalog$open$main, x) |> c()
}

#' @autoglobal
#' @noRd
open_group <- function(x, call = caller_env()) {
  switch(
    x,
    profile = list(
      group = "Open Payments Profiles",
      alias = c(
        "PROF_covered",
        "PROF_physician",
        "PROF_information",
        "PROF_mapping",
        "PROF_entity",
        "PROF_teaching"
      )
    ),
    summary = list(
      group = "Open Payments Summaries",
      alias = c(
        "SUMM_dashboard",
        "SUMM_state_all",
        "SUMM_state_group",
        "SUMM_nation_all",
        "SUMM_nation_group"
      )
    ),
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
}

#' @autoglobal
#' @noRd
open_temp <- function(x, call = caller_env()) {
  x <- switch(
    x,
    DATA_general        = "^General Payment Data$",
    DATA_ownership      = "^Ownership Payment Data$",
    DATA_research       = "^Research Payment Data$",
    GROUP_recip_nature  = "^Payments Grouped by Covered Recipient and Nature of Payments$",
    GROUP_recip_entity  = "^Payments Grouped by Covered Recipient and Reporting Entities$",
    GROUP_all           = "^Payments Grouped by Reporting Entities, Covered Recipient, and Nature of Payments$",
    GROUP_entity_nature = "^Payments Grouped by Covered Recipient and Nature of Payments$",
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )

  if (!exists("catalog")) .catalog <- catalogs()

  select_alias(.catalog$open$temp, x)
}

#' @autoglobal
#' @noRd
open_temp_group <- function(x, call = caller_env()) {
  switch(
    x,
    grouped_payment = list(
      group = "Payments Grouped by Year",
      alias = c(
        "GROUP_recip_nature",
        "GROUP_recip_entity",
        "GROUP_entity_nature",
        "GROUP_all"
      )
    ),
    detailed_payment = list(
      group = "Payments Detailed by Year",
      alias = c(
        "DATA_general",
        "DATA_ownership",
        "DATA_research"
      )
    ),
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
}
