#' @autoglobal
#' @noRd
select_open <- function(x, call = caller_env()) {
  x <- switch(
    x,
    profile_covered      = "^Covered Recipient Profile Supplement$",
    profile_physician    = "^Physician \\(Distinct\\) Profile Information$",
    profile_information  = "^Profile Information$",
    profile_mapping      = "^Provider Profile ID Mapping Table$",
    profile_entity       = "^Reporting Entity Profile Information$",
    profile_teaching     = "^Teaching Hospital Profile Information$",
    summary_dashboard    = "^Summary Dashboard",
    summary_state        = "^State Level Payment Total and Averages for all Years$",
    summary_nature       = "^State Payment Totals and Averages Grouped by Nature of Payment for all Years$",
    summary_national     = "^National Level Payment Total and Averages for all Years$",
    summary_specialty    = "^National Level Payment Total and Averages by Provider Specialty for all Years$",
    cli::cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )

  if (!exists("catalog")) .catalog <- catalogs()

  res <- select_alias(.catalog$open$main, x)

  if (empty(res))     cli::cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  if (nrow(res) > 1L) cli::cli_abort(c("x" = "> 1 match found for {.val {x}}."), call = call)

  c(res)
}

#' @autoglobal
#' @noRd
select_open_group <- function(x, call = caller_env()) {
  switch(
    x,
    profile = list(
      group = "Open Payments Profiles",
      alias = c(
        "profile_covered",
        "profile_physician",
        "profile_information",
        "profile_mapping",
        "profile_entity",
        "profile_teaching"
      )
    ),
    summary = list(
      group = "Open Payments Summaries",
      alias = c(
        "summary_dashboard",
        "summary_state",
        "summary_nature",
        "summary_national",
        "summary_specialty"
      )
    ),
    cli::cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
}

#' @autoglobal
#' @noRd
select_open_temp <- function(x, call = caller_env()) {
  x <- switch(
    x,
    payment_general               = "^General Payment Data$",
    payment_ownership             = "^Ownership Payment Data$",
    payment_research              = "^Research Payment Data$",
    grouped_covered_nature        = "^Payments Grouped by Covered Recipient and Nature of Payments$",
    grouped_covered_entity        = "^Payments Grouped by Covered Recipient and Reporting Entities$",
    grouped_entity_nature         = "^Payments Grouped by Reporting Entities and Nature of Payments$",
    grouped_entity_covered_nature = "^Payments Grouped by Reporting Entities, Covered Recipient, and Nature of Payments$",
    grouped_state_nature          = "^State Payment Totals Grouped by Nature of Payment for all Years$",
    cli::cli_abort(c("x"          = "No matches found for {.val {x}}."), call = call)
  )

  if (!exists("catalog")) .catalog <- catalogs()

  res <- select_alias(.catalog$open$temp, x)

  if (empty(res)) cli::cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)

  list_tidy(
    !!!c(slt(res, -data)),
    endpoints   = get_elem(res, "data") |> _[[1]],
    identifier  = endpoints$identifier[1]
  )
}

#' @autoglobal
#' @noRd
select_open_troup <- function(x, call = caller_env()) {
  switch(
    x,
    payment_grouped = list(
      group = "Open Payments by Year (Grouped)",
      alias = c(
        "grouped_covered_nature",
        "grouped_covered_entity",
        "grouped_entity_nature",
        "grouped_entity_covered_nature",
        "grouped_state_nature"
      )
    ),
    payment_detailed = list(
      group = "Open Payments by Year (Detailed)",
      alias = c(
        "payment_general",
        "payment_ownership",
        "payment_research"
      )
    ),
    cli::cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
}
