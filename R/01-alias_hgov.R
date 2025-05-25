#' @include S7_classes.R
NULL

#' Healthcare.Gov API Endpoints
#' @name hgov
#' @param alias `<chr>` endpoint or group alias
#' @param call `<env>` environment to use for error reporting
#' @returns An S7 `<class_endpoint>` or `<class_temporal>` object
#' @examples
#' hgov_endpoint("ab_reg_comp")
#' hgov_temporal("puf_rate")
NULL

#' @rdname hgov
#' @autoglobal
#' @export
hgov_endpoint <- function(alias, call = caller_env()) {

  check_required(alias)

  x <- switch(
    alias,
    auto_pop_file            = "^Auto[-]population File$",
    ab_reg_comp              = "^AB Registration Completion List$",
    ab_sus_term              = "^AB Suspension and Termination List$",
    ab_reg_gloss             = "^Agent Broker Registration Tracker Glossary$",
    ab_reg_trac              = "^Agent Broker Registration Tracker$",
    catastrophic_plans       = "^Catastrophic Plans for People with Cancelled Policies$",
    contact_admins           = "^Helpful Contacts Admins$",
    counties                 = "^Counties$",
    county_service_areas     = "^County Service Areas$",
    issuer_partner_lookup    = "^Issuer[_]Partner[_]Lookup$",
    issuer_partner_reference = "^Issuer Partner Directory [-] Reference Text$",
    issuer_partner_directory = "^Issuer and DE Partner Directory$",
    partner                  = "^Direct Enrollment Partners$",
    nipr_valid_authority     = "^NIPR Valid Lines of Authority List$",
    nipr_valid_state         = "^Marketplace Agent[/]Broker NIPR Valid Lines of Authority[,] by State$",
    response_codes           = "^Response Codes$",
    rolling_draft_ecp        = "^Rolling Draft ECP List$",
    slcsp_cnty_zip           = "^SLCSP [-] County[-]Zip Reference Data$",
    states                   = "^States$",
    local_help               = "^Find Local Help$",
    cli::cli_abort(c("x"     = "{.emph alias} {.val {alias}} is invalid."), call = call)
    )

  res <- select_alias(the$catalogs$hgov$main, x)

  if (empty(res))     cli::cli_abort(c("x" = "{.val {x}} returned no matches."), call = call)
  if (nrow(res) > 1L) cli::cli_abort(c("x" = "{.val {x}} returned more than 1 match."), call = call)

  x <- c(res)

  class_endpoint(
    identifier  = x$identifier,
    metadata    = get_metadata(x),
    dimensions  = get_dimensions(x)
  )
}

#' @rdname hgov
#' @autoglobal
#' @export
hgov_temporal <- function(alias, call = caller_env()) {

  check_required(alias)

  x <- switch(
    alias,
    mlr_datasets  = "^MLR Dataset$",
    puf_benefits  = "^Benefits and Cost Sharing PUF$",
    puf_business  = "^Business Rules PUF$",
    puf_machine   = "^Machine Readable PUF$",
    puf_network   = "^Network PUF$",
    puf_plan_attr = "^Plan Attributes PUF$",
    puf_plan_walk = "^Plan ID Crosswalk PUF$",
    puf_rate      = "^Rate PUF$",
    puf_service   = "^Service Area PUF$",
    puf_tic       = "^Transparency in Coverage PUF$",
    cli::cli_abort(c("x" = "{.emph alias} {.val {alias}} is invalid."), call = call)
    )

  res <- select_alias(the$catalogs$hgov$temp, x)

  if (empty(res)) cli::cli_abort(c("x" = "{.val {x}} returned no matches."), call = call)

  x <- list_tidy(
    !!!c(slt(res, -endpoints)),
    endpoints   = pluck(get_elem(res, "endpoints"), 1),
    identifier  = endpoints$identifier[1]
  )

  class_temporal(
    metadata    = get_metadata(x),
    dimensions  = get_dimensions(x),
    endpoints   = x$endpoints
  )
}
