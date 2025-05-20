#' @autoglobal
#' @noRd
select_hgov <- function(x, call = caller_env()) {
  x <- switch(
    x,
    ab_registration_completion         = "^AB Registration Completion List$",
    ab_suspension_termination          = "^AB Suspension and Termination List$",
    agent_broker_registration_glossary = "^Agent Broker Registration Tracker Glossary$",
    agent_broker_registration_tracker  = "^Agent Broker Registration Tracker$",
    authority_state                    = "^Marketplace Agent[/]Broker NIPR Valid Lines of Authority[,] by State$",
    auto_pop_file                      = "^Auto[-]population File$",
    benefits_cost_sharing              = "^Benefits and Cost Sharing PUF$",
    business_rules                     = "^Business Rules PUF$",
    catastrophic_plans                 = "^Catastrophic Plans for People with Cancelled Policies$",
    contact_admins                     = "^Helpful Contacts Admins$",
    counties                           = "^Counties$",
    county_service_areas               = "^County Service Areas$",
    direct_enrollment_partners         = "^Direct Enrollment Partners$",
    issuer_partner_lookup              = "^Issuer[_]Partner[_]Lookup$",
    issuer_partner_directory           = "^Issuer Partner Directory [-] Reference Text$",
    issuer_de_partner_directory        = "^Issuer and DE Partner Directory$",
    network_puf                        = "^Network PUF$",
    nipr_valid_authority               = "^NIPR Valid Lines of Authority List$",
    plan_attribute                     = "^Plan Attributes PUF$",
    plan_id_cross                      = "^Plan ID Crosswalk PUF$",
    response_codes                     = "^Response Codes$",
    rolling_draft_ecp                  = "^Rolling Draft ECP List$",
    service_area                       = "^Service Area PUF$",
    slcsp_cnty_zip                     = "^SLCSP [-] County[-]Zip Reference Data$",
    states                             = "^States$",
    qhp_ind_med                        = "^QHP Landscape Individual Market Medical$",
    qhp_ind_dent                       = "^QHP Landscape Individual Market Dental$",
    qhp_shop_med                       = "^QHP Landscape SHOP Market Medical$",
    qhp_shop_dent                      = "^QHP Landscape SHOP Market Dental$",
    qhp_id_shop_med                    = "^QHP Landscape ID SHOP Market Medical$",
    qhp_id_shop_dent                   = "^QHP Landscape ID SHOP Market Dental$",
    qhp_id_ind_dent                    = "^QHP Landscape ID Individual Market Dental$",
    qhp_id_ind_med                     = "^QHP Landscape ID Individual Market Medical$",
    qhp_nm_ind_dent                    = "^QHP Landscape NM Individual Market Dental$",
    qhp_nm_ind_med                     = "^QHP Landscape NM Individual Market Medical$",
    qhp_bus_rule_variables             = "^QHP Landscape Health Plan Business Rule Variables$",
    qhp_py19_shop_med_instruct         = "^QHP PY19 Medical SHOP Landscape Instructions$",
    qhp_py19_ind_med_instruct          = "^QHP PY19 Medical Individual Landscape Instructions$",
    qhp_py19_ind_dent_instruct         = "^QHP PY19 Dental Individual Landscape Instructions$",
    qhp_py18_shop_med_instruct         = "^QHP PY18 Medical SHOP Landscape Instructions$",
    qhp_py18_ind_med_instruct          = "^QHP PY18 Medical Individual Landscape Instructions$",
    qhp_py18_ind_dent_instruct         = "^QHP PY18 Dental Individual Landscape Instructions$",
    local_help                         = "^Find Local Help$",
    cli::cli_abort(c("x" = "No matches found for {.val {x}}."), call = call))

  res <- select_alias(the$catalogs$hgov$main, x)

  if (empty(res))     cli::cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  if (nrow(res) > 1L) cli::cli_abort(c("x" = "> 1 match found for {.val {x}}."), call = call)

  c(res)
}

#' @autoglobal
#' @noRd
select_hgov_temp <- function(x, call = caller_env()) {

  x <- switch(
    x,
    medical_loss_ratio     = "^MLR Dataset$",
    qhp_quality_ratings    = "^Quality PUF$",
    hie_benefits_costshare = "^Benefits and Cost Sharing PUF$",
    hie_business_rules     = "^Business Rules PUF$",
    hie_machine_readable   = "^Machine Readable PUF$",
    hie_network            = "^Network PUF$",
    hie_plan_attributes    = "^Plan Attributes PUF$",
    hie_plan_id_crosswalk  = "^Plan ID Crosswalk PUF$",
    hie_rate               = "^Rate PUF$",
    hie_service_area       = "^Service Area PUF$",
    hie_transparency       = "^Transparency in Coverage PUF$",
    cli::cli_abort(c("x"   = "No matches found for {.val {x}}."), call = call))

  res <- select_alias(the$catalogs$hgov$temp, x)

  if (empty(res)) cli::cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)

  list_tidy(
    !!!c(slt(res, -data)),
    endpoints   = get_elem(res, "data") |> _[[1]] |> slt(-contact),
    identifier  = endpoints$identifier[1]
  )
}
