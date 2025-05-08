#' @autoglobal
#' @noRd
select_open <- function(x, call = caller_env()) {
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

  res <- select_alias(.catalog$open$main, x)

  if (empty(res))     cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  if (nrow(res) > 1L) cli_abort(c("x" = "> 1 match found for {.val {x}}."), call = call)

  c(res)
}

#' @autoglobal
#' @noRd
select_open_temp <- function(x, call = caller_env()) {
  x <- switch(
    x,
    DATA_general        = "^General Payment Data$",
    DATA_ownership      = "^Ownership Payment Data$",
    DATA_research       = "^Research Payment Data$",
    GROUP_recip_nature  = "^Payments Grouped by Covered Recipient and Nature of Payments$",
    GROUP_recip_entity  = "^Payments Grouped by Covered Recipient and Reporting Entities$",
    GROUP_entity_nature = "^Payments Grouped by Reporting Entities and Nature of Payments$",
    GROUP_all           = "^Payments Grouped by Reporting Entities, Covered Recipient, and Nature of Payments$",
    GROUP_state_nature  = "^State Payment Totals Grouped by Nature of Payment for all Years$",
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )

  if (!exists("catalog")) .catalog <- catalogs()

  res <- select_alias(.catalog$open$temp, x)

  if (empty(res)) cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)

  list(
    title       = res$title[1],
    description = res$description[1],
    modified    = res$modified[1],
    identifier  = res$data[[1]][["identifier"]][1],
    endpoints   = slt(res$data[[1]], year, identifier, download)
  )
}
