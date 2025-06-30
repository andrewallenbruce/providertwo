#' @autoglobal
#' @noRd
alias_info <- function(x) {

  check_required(x)
  check_character(x, allow_na = FALSE, allow_null = FALSE)

  flist(
    aka = x,
    api = api_type(x),
    clg = catalog_type(x),
    rex = alias_regex(x),
    exp = glue("the$catalog${clg}${api}"),
    i   = select_alias(exp, rex)
  )
}

#' @autoglobal
#' @noRd
check_alias_results <- function(x, aka, rex) {

  if (is_empty(x)) {
    cli_abort(c("x" = "{.field {aka}} ({.val {rex}}) had {nrow(x)} matches."))
  }

  if (nrow(x) > 1L) {
    cli_abort(c(
      "x" = "{.field {aka}} ({.val {rex}}) had {nrow(x)} matches:",
      cli::col_yellow(cli::format_bullets_raw(x$title))))
  }

}

#' @autoglobal
#' @noRd
fmt_tmp <- function(x) {
  # end <- yank(get_elem(res, "endpoints"))
  # if (all_na(gv(end, "resources"))) gv(end, "resources") <- NULL
  # Contries[-match(c("France", "Mali"), Contries)]
  flist(
    !!!c(x[names(x) %!=% "endpoints"]),
    endpoints  = yank(x$endpoints),
    identifier = yank(endpoints$identifier)
  )
}

#' @autoglobal
#' @noRd
as_api <- function(api, end, tmp) {
  switch(
    api,
    end = class_endpoint(end),
    tmp = class_temporal(tmp),
    cli_abort(c("x" = "{.field {api}} unrecognized."))
    )
}

#' API Endpoints
#' @param alias `<chr>` endpoint alias
#' @returns An S7 object
#' @examplesIf rlang::is_interactive()
#' endpoint("care_dial_end")
#' endpoint("care_dial_tmp")
#' endpoint("quality_payment")
#' endpoint("managed_longterm")
#' endpoint("state_drug_util")
#' endpoint("hgov_ab_reg_comp")
#' endpoint("hgov_puf_rate")
#' endpoint("profile_covered")
#' endpoint("grouped_state_nature")
#' endpoint("asc_facility")
#' endpoint("dialysis_by_facility")
#' @autoglobal
#' @export
endpoint <- function(alias) {

  .c(aka, api, clg, rex, exp, i) %=% alias_info(alias)

  check_alias_results(i, aka, rex)

  i <- switch(api, end = c(i), tmp = fmt_tmp(i))

  switch(
    clg,
    care = class_care(
      metadata   = get_metadata(i),
      dimensions = get_dimensions(i, clg, api),
      identifier = switch(api,
        end = class_endpoint(i$identifier),
        tmp = class_temporal(slt(i$endpoints, -resources))),
      resources  = switch(api,
        end = class_endpoint(i$resources),
        tmp = class_temporal(slt(i$endpoints, year, resources)))
    ),
    caid = class_caid(
      metadata   = get_metadata(i),
      dimensions = get_dimensions(i, clg),
      identifier = switch(api,
        end = class_endpoint(i$identifier),
        tmp = class_temporal(i$endpoints)
      )
    ),
    prov = class_prov(
      metadata   = get_metadata(i),
      dimensions = get_dimensions(i, clg),
      identifier = switch(api,
        end = class_endpoint(i$identifier),
        tmp = class_temporal(i$endpoints)
      )
    ),
    open = class_open(
      metadata   = get_metadata(i),
      dimensions = get_dimensions(i, clg),
      identifier = switch(api,
        end = class_endpoint(i$identifier),
        tmp = class_temporal(i$endpoints)
      )
    ),
    hgov = class_hgov(
      metadata   = get_metadata(i),
      dimensions = get_dimensions(i, clg),
      identifier = switch(api,
        end = class_endpoint(i$identifier),
        tmp = class_temporal(i$endpoints)
      )
    )
  )
}

#' Create Collection of Endpoints
#'
#' @param alias `<chr>` Alias representing the CMS data endpoint.
#' @returns S7 `class_collection` object.
#' @examplesIf rlang::is_interactive()
#' collection("caid_demographics")
#' collection("caid_unwind")
#' collection("caid_services")
#' @autoglobal
#' @export
collection <- function(alias) {
  x <- group_regex(alias)

  class_collection(name    = x$name,
                   members = names_map(x$alias, new_endpoint))
}

#' Create Groups of Endpoints
#'
#' @param alias `<chr>` Alias representing the CMS data endpoint.
#' @param desc `<chr>` Name of the group.
#' @returns S7 `class_group` object.
#' @examplesIf rlang::is_interactive()
#' group(c("cahps_hhc_patient", "hgov_qhp_business"))
#' group(c("hgov_local_help", "hgov_qhp_business"))
#' @autoglobal
#' @export
group <- function(alias, desc = NULL) {
  class_group(
    name    = desc %||% paste0(alias, collapse = " | "),
    members = names_map(alias, new_endpoint)
  )
}
