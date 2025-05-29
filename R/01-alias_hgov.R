#' @include S7_classes.R
NULL

#' @autoglobal
#' @noRd
hgov_dimensions <- function(x, call = caller_env()) {

  x <- x$identifier |>
    request() |>
    req_url_query(offset = 0L, limit = 1L, results = "false") |>
    req_error(is_error = ~ FALSE) |>
    perform_simple()

  class_dimensions(
    limit  = 500L,
    rows   = x$count %||% 0L,
    fields = x$query$properties %||% new_list(length = 1L, default = x$message))
}

#' Healthcare.Gov API Endpoints
#' @name hgov
#' @param alias `<chr>` endpoint or group alias
#' @param call `<env>` environment to use for error reporting
#' @returns An S7 `<class_endpoint>` or `<class_temporal>` object
#' @examples
#' hgov_endpoint("hgov_ab_reg_comp")
#' hgov_temporal("hgov_puf_rate")
NULL

#' @rdname hgov
#' @autoglobal
#' @export
hgov_endpoint <- function(alias, call = caller_env()) {

  check_required(alias)

  x <- switch(
    alias,
    hgov_auto_pop           = "^Auto[-]population File$",
    hgov_ab_reg_comp        = "^AB Registration Completion List$",
    hgov_ab_sus_term        = "^AB Suspension and Termination List$",
    hgov_ab_reg_gloss       = "^Agent Broker Registration Tracker Glossary$",
    hgov_ab_reg_trac        = "^Agent Broker Registration Tracker$",
    hgov_catastrophic       = "^Catastrophic Plans for People with Cancelled Policies$",
    hgov_contact_admin      = "^Helpful Contacts Admins$",
    hgov_counties           = "^Counties$",
    hgov_county_service     = "^County Service Areas$",
    hgov_partner_lookup     = "^Issuer[_]Partner[_]Lookup$",
    hgov_partner_reference  = "^Issuer Partner Directory [-] Reference Text$",
    hgov_partner_directory  = "^Issuer and DE Partner Directory$",
    hgov_partner_enrollment = "^Direct Enrollment Partners$",
    hgov_nipr_authority     = "^NIPR Valid Lines of Authority List$",
    hgov_nipr_state         = "^Marketplace Agent[/]Broker NIPR Valid Lines of Authority[,] by State$",
    hgov_response_codes     = "^Response Codes$",
    hgov_rolling_draft      = "^Rolling Draft ECP List$",
    hgov_slcsp_county       = "^SLCSP [-] County[-]Zip Reference Data$",
    hgov_states             = "^States$",
    hgov_local_help         = "^Find Local Help$",
    hgov_qhp_consumer       = "QHP Selections by Type of Consumer and County",
    hgov_qhp_aptc           = "QHP Selections by APTC and County",
    hgov_qhp_csr            = "QHP Selections by CSR and County",
    hgov_qhp_metal          = "QHP Selections by Metal Level and County",
    hgov_qhp_income         = "QHP Selections by Household Income as a Percent of the Federal Poverty Level and County",
    hgov_qhp_ethnicity      = "QHP Selections by Race/Ethnicity and County",
    hgov_qhp_age            = "QHP Selections by Age Group and County",
    hgov_qhp_business       = "QHP Landscape Health Plan Business Rule Variables",
    cli::cli_abort(c("x"    = "{.emph alias} {.val {alias}} is invalid."), call = call)
  )

  res <- select_alias(the$catalogs$hgov$main, x)

  if (empty(res))     cli::cli_abort(c("x" = "{.val {x}} returned no matches."), call = call)
  if (nrow(res) > 1L) cli::cli_abort(c("x" = "{.val {x}} returned more than 1 match."), call = call)

  x <- c(res)

  class_endpoint(
    identifier  = x$identifier,
    metadata    = get_metadata(x),
    dimensions  = hgov_dimensions(x)
  )
}

#' @rdname hgov
#' @autoglobal
#' @export
hgov_temporal <- function(alias, call = caller_env()) {

  check_required(alias)

  x <- switch(
    alias,
    hgov_mlr             = "^MLR Dataset$",
    hgov_puf_benefits    = "^Benefits and Cost Sharing PUF$",
    hgov_puf_business    = "^Business Rules PUF$",
    hgov_puf_machine     = "^Machine Readable PUF$",
    hgov_puf_network     = "^Network PUF$",
    hgov_puf_plan_attr   = "^Plan Attributes PUF$",
    hgov_puf_plan_walk   = "^Plan ID Crosswalk PUF$",
    hgov_puf_rate        = "^Rate PUF$",
    hgov_puf_service     = "^Service Area PUF$",
    hgov_puf_tic         = "^Transparency in Coverage PUF$",
    # THESE CONTAIN ZIP FILES NOT ENDPOINTS
    hgov_qhp_ind_dnt     = "QHP Landscape Individual Market Dental",
    hgov_qhp_ind_med     = "QHP Landscape Individual Market Medical",
    hgov_qhp_shop_dnt    = "QHP Landscape SHOP Market Dental",
    hgov_qhp_shop_med    = "QHP Landscape SHOP Market Medical",
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
    dimensions  = hgov_dimensions(x),
    endpoints   = x$endpoints
  )
}
