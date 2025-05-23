#' @include S7_classes.R
NULL

#' Open Payments API Endpoints
#' @name openpayments
#' @param alias `<chr>` endpoint or group alias
#' @param call `<env>` environment to use for error reporting
#' @param ... Additional arguments passed to the group constructor
#' @returns An S7 `<class_endpoint>`, `<class_temporal>` or `<class_group>` object
#' @examples
#' open_endpoint("profile_covered")
#' open_temporal("grouped_state_nature")
#' open_group("payment_detailed")
NULL

#' @autoglobal
#' @rdname openpayments
#' @export
open_endpoint <- function(alias, call = caller_env()) {

  check_required(alias)

  x <- switch(
    alias,
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
    cli::cli_abort(c("x" = "{.emph alias} {.val {alias}} is invalid."), call = call)
  )

  res <- select_alias(the$catalogs$open$main, x)

  if (empty(res))     cli::cli_abort(c("x" = "{.val {x}} returned no matches."), call = call)
  if (nrow(res) > 1L) cli::cli_abort(c("x" = "{.val {x}} returned more than 1 match."), call = call)

  x <- c(slt(res, -contact))

  class_endpoint(
    identifier  = x$identifier,
    metadata    = get_metadata(x),
    dimensions  = get_dimensions(x)
  )
}

#' @autoglobal
#' @rdname openpayments
#' @export
open_temporal <- function(alias, call = caller_env()) {

  check_required(alias)

  x <- switch(
    alias,
    payment_general               = "^General Payment Data$",
    payment_ownership             = "^Ownership Payment Data$",
    payment_research              = "^Research Payment Data$",
    grouped_covered_nature        = "^Payments Grouped by Covered Recipient and Nature of Payments$",
    grouped_covered_entity        = "^Payments Grouped by Covered Recipient and Reporting Entities$",
    grouped_entity_nature         = "^Payments Grouped by Reporting Entities and Nature of Payments$",
    grouped_entity_covered_nature = "^Payments Grouped by Reporting Entities, Covered Recipient, and Nature of Payments$",
    grouped_state_nature          = "^State Payment Totals Grouped by Nature of Payment for all Years$",
    cli::cli_abort(c("x"          = "{.emph alias} {.val {alias}} is invalid."), call = call)
  )

  res <- select_alias(the$catalogs$open$temp, x)

  if (empty(res)) cli::cli_abort(c("x" = "{.val {x}} returned no matches."), call = call)

  x <- list_tidy(
    !!!c(slt(res, -endpoints)),
    endpoints   = pluck(get_elem(res, "endpoints"), 1) |> slt(-contact),
    identifier  = endpoints$identifier[1]
  )

  class_temporal(
    metadata    = get_metadata(x),
    dimensions  = get_dimensions(x),
    endpoints   = x$endpoints
  )
}

#' @autoglobal
#' @rdname openpayments
#' @export
open_group <- function(alias, call = caller_env(), ...) {

  check_required(alias)

  x <- switch(
    alias,
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
    cli::cli_abort(c("x" = "{.emph group alias} {.val {alias}} is invalid."), call = call)
  )

  new_group(
    member_names = x$alias,
    group_name   = x$group,
    ...
  )
}
