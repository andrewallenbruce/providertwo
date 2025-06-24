#' @autoglobal
#' @noRd
alias_info <- function(x) {
  flist(
    alias   = x,
    api     = api_type(x),
    catalog = catalog_type(x),
    regex   = alias_regex(x),
    str     = str2lang(glue("the$catalog${catalog}${api}"))
  )
}

#' API Endpoints
#' @param alias `<chr>` endpoint alias
#' @returns An S7 object
#' @examplesIf rlang::is_interactive()
#' new_endpoint("care_dial_end")
#' new_endpoint("care_dial_tmp")
#' new_endpoint("quality_payment")
#' new_endpoint("managed_longterm")
#' new_endpoint("state_drug_util")
#' new_endpoint("hgov_ab_reg_comp")
#' new_endpoint("hgov_puf_rate")
#' new_endpoint("profile_covered")
#' new_endpoint("grouped_state_nature")
#' new_endpoint("asc_facility")
#' new_endpoint("prov_dialysis")
#' @autoglobal
#' @export
new_endpoint <- function(alias) {

  check_required(alias)
  check_character(alias, allow_na = FALSE, allow_null = FALSE)

  x   <- alias_info(alias)

  res <- select_alias(eval(x$str), x$regex)

  if (is_empty(res))  cli_abort(c("x" = "{.val {x$regex}} has 0 matches."))
  if (nrow(res) > 1L) cli_abort(c("x" = "{.val {x$regex}} has multiple matches."))

  # end <- yank(get_elem(res, "endpoints"))
  # if (all_na(gv(end, "resources"))) gv(end, "resources") <- NULL

  a <- switch(
    x$api,
    end = c(res),
    tmp = flist(
      !!!c(slt(res, -endpoints)),
      endpoints = yank(get_elem(res, "endpoints")),
      identifier = yank(get_elem(endpoints, "identifier"))
    )
  )

  switch(
    x$catalog,
    care = class_care(
      metadata   = get_metadata(a),
      dimensions = get_dimensions(a, x$catalog, x$api),
      identifier = if (x$api == "end")
        class_endpoint(a$identifier)
      else
        class_temporal(slt(a$endpoints, -resources)),
      resources  = if (x$api == "end")
        class_endpoint(a$resources)
      else
        class_temporal(slt(a$endpoints, year, resources))
    ),
    caid = class_caid(
      metadata   = get_metadata(a),
      dimensions = get_dimensions(a, x$catalog, x$api),
      identifier = if (x$api == "end")
        class_endpoint(a$identifier)
      else
        class_temporal(a$endpoints)
    ),
    prov = class_prov(
      metadata   = get_metadata(a),
      dimensions = get_dimensions(a, x$catalog, x$api),
      identifier = if (x$api == "end")
        class_endpoint(a$identifier)
      else
        class_temporal(a$endpoints)
    ),
    open = class_open(
      metadata   = get_metadata(a),
      dimensions = get_dimensions(a, x$catalog, x$api),
      identifier = if (x$api == "end")
        class_endpoint(a$identifier)
      else
        class_temporal(a$endpoints)
    ),
    hgov = class_hgov(
      metadata   = get_metadata(a),
      dimensions = get_dimensions(a, x$catalog, x$api),
      identifier = if (x$api == "end")
        class_endpoint(a$identifier)
      else
        class_temporal(a$endpoints)
    ),
  )
}

#' Create Collection of Endpoints
#'
#' @param alias `<chr>` Alias representing the CMS data endpoint.
#' @returns S7 `class_collection` object.
#' @examplesIf rlang::is_interactive()
#' new_collection("caid_demographics")
#' new_collection("caid_unwind")
#' new_collection("caid_services")
#' @autoglobal
#' @export
new_collection <- function(alias) {

  x <- group_regex(alias)

  class_collection(
    name = x$name,
    members = set_names(map(x$alias, new_endpoint), x$alias))
}

#' Create Groups of Endpoints
#'
#' @param aliases `<chr>` Alias representing the CMS data endpoint.
#' @param description    `<chr>` Name of the group.
#' @returns S7 `class_group` object.
#' @examplesIf rlang::is_interactive()
#' new_group(c("cahps_hhc_patient", "hgov_qhp_business"))
#' new_group(c("hgov_local_help", "hgov_qhp_business"))
#' @autoglobal
#' @export
new_group <- function(aliases, description = NULL) {
  class_group(
    name = description %||% paste0(aliases, collapse = " | "),
    members = set_names(map(aliases, new_endpoint), aliases))
}
