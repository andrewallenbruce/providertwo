# open_main("profile_covered")
# open_main("profile_physician")
# open_main("profile_information")
# open_main("profile_mapping")
# open_main("profile_entity")
# open_main("profile_teaching")
# open_main("dashboard")
# open_main("state_total")
# open_main("state_group")
# open_main("national_group")
# open_main("national_total")
#' @autoglobal
#' @noRd
open_main <- function(x, call = caller_env()) {
  x <- switch(
    x,
    profile_covered      = "^Covered Recipient Profile Supplement",
    profile_physician    = "^Physician \\(Distinct\\) Profile Information",
    profile_information  = "^Profile Information",
    profile_mapping      = "^Provider Profile ID Mapping Table",
    profile_entity       = "^Reporting Entity Profile Information",
    profile_teaching     = "^Teaching Hospital Profile Information",
    dashboard            = "^Summary Dashboard",
    state_total          = "^State Level Payment Total and Averages for all Years$",
    state_group          = "^State Payment Totals and Averages Grouped by Nature of Payment for all Years$",
    national_group       = "^National Level Payment Total and Averages by Provider Specialty for all Years$",
    national_total       = "^National Level Payment Total and Averages for all Years$",
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )

  if (!exists("catalog")) .catalog <- catalogs()

  select_alias(.catalog$open$main, x) |> c()
}

# open_temp("general")
# open_temp("ownership")
# open_temp("research")
#' @noRd
open_temp <- function(x, call = caller_env()) {
  x <- switch(
    x,
    general   = "^General Payment Data$",
    ownership = "^Ownership Payment Data$",
    research  = "^Research Payment Data$",
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call))

  if (!exists("catalog")) .catalog <- catalogs()

  select_alias(.catalog$open$temp, x)
}

# open_temp_group("grouped_payments")
#' @noRd
open_temp_group <- function(x, call = caller_env()) {
  x <- switch(
    x,
    "grouped_payments" = "^Payments Grouped by",
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call))

  if (!exists("catalog"))
    .catalog <- catalogs()

  select_alias(.catalog$open$temp, x)
}

# "recipient_nature",        "^Payments Grouped by Covered Recipient and Nature of Payments$",
# "recipient_entity",        "^Payments Grouped by Covered Recipient and Reporting Entities$",
# "entity_nature",           "^Payments Grouped by Reporting Entities and Nature of Payments$",
# "entity_recipient_nature", "^Payments Grouped by Reporting Entities, Covered Recipient, and Nature of Payments$",
## open_temp("recipient_nature")
## open_temp("recipient_entity")
## open_temp("entity_nature")
## open_temp("entity_recipient_nature")
