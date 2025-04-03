#' @examplesIf rlang::is_interactive()
#' open_main("prof_cov")
#' open_main("prof_phys")
#' open_main("prof_info")
#' open_main("prof_map")
#' open_main("prof_entity")
#' open_main("prof_teach")
#' open_main("dashboard")
#' open_main("pay_state_total")
#' open_main("pay_state_group")
#' open_main("pay_nat_group")
#' open_main("pay_nat_total")
#' @autoglobal
#' @noRd
open_main <- function(x) {

  x <- nswitch(
    x,
    "prof_cov",        "^Covered Recipient Profile Supplement",
    "prof_phys",       "^Physician \\(Distinct\\) Profile Information",
    "prof_info",       "^Profile Information",
    "prof_map",        "^Provider Profile ID Mapping Table",
    "prof_entity",     "^Reporting Entity Profile Information",
    "prof_teach",      "^Teaching Hospital Profile Information",
    "dashboard",       "^Summary Dashboard",
    "pay_state_total", "^State Level Payment Total and Averages for all Years$",
    "pay_state_group", "^State Payment Totals and Averages Grouped by Nature of Payment for all Years$",
    "pay_nat_group",   "^National Level Payment Total and Averages by Provider Specialty for all Years$",
    "pay_nat_total",   "^National Level Payment Total and Averages for all Years$",
    default = NA_character_,
    nThread = 4L
  )

  if (na(x)) cli_abort("x" = "No matches found.", call = call)

  if (!exists("catalog")) catalog <- catalogs()

  select_alias(catalog$open$current, x) |> c()
}

#' @examplesIf rlang::is_interactive()
#' open_temp("general")
#' open_temp("ownership")
#' open_temp("research")
#' open_temp("recipient_nature")
#' open_temp("recipient_entity")
#' open_temp("entity_nature")
#' open_temp("entity_recipient_nature")
#' open_temp("state_nature")
#' @noRd
open_temp <- function(x) {

  x <- nswitch(
    x,
    "general",                 "^General Payment Data$",
    "ownership",               "^Ownership Payment Data$",
    "research",                "^Research Payment Data$",
    "recipient_nature",        "^Payments Grouped by Covered Recipient and Nature of Payments$",
    "recipient_entity",        "^Payments Grouped by Covered Recipient and Reporting Entities$",
    "entity_nature",           "^Payments Grouped by Reporting Entities and Nature of Payments$",
    "entity_recipient_nature", "^Payments Grouped by Reporting Entities, Covered Recipient, and Nature of Payments$",
    "state_nature",            "^State Payment Totals Grouped by Nature of Payment for all Years$",
    default = NA_character_,
    nThread = 4L
  )

  if (na(x)) cli_abort("x" = "No matches found.", call = call)

  if (!exists("catalog")) catalog <- catalogs()

  select_alias(catalog$open$temporal, x)
}
