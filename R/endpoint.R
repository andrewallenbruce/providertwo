#' @autoglobal
#' @noRd
check_alias_results <- function(x, call = caller_env()) {
  if (is_empty(x$tbl)) {
    cli_abort(
      c("x" = "{.field {x$aka}} ({.val {x$rex}}) had {nrow(x$tbl)} matches."),
      call = call)
  }

  if (nrow(x$tbl) > 1L) {
    cli_abort(
      c("x" = "{.field {x$aka}} ({.val {x$rex}}) had {nrow(x$tbl)} matches:",
        cli::col_yellow(cli::format_bullets_raw(x$tbl$title))
      ),
      call = call
    )
  }

}

#' @autoglobal
#' @noRd
select_alias <- function(x, alias, ...) {
  subset_detect(i = eval(str2lang(x)),
                j = title,
                p = alias,
                ...)
}

#' @autoglobal
#' @noRd
fmt_tmp <- function(x) {
  # if (all_na(gv(end, "resources")))
  # gv(end, "resources") <- NULL
  flist(
    !!!c(x[names(x) %!=% "endpoints"]),
    endpoints  = yank(x$endpoints),
    identifier = yank(endpoints$identifier)
  )
}

#' @autoglobal
#' @noRd
alias_lookup <- function(x) {
  check_required(x)

  x <- flist(
    aka = x,
    api = api_type(x),
    clg = catalog_type(x),
    rex = alias_regex(x),
    exp = glue("the$catalog${clg}${api}"),
    tbl = select_alias(exp, rex)
  )

  check_alias_results(x)

  list_combine(
    list_modify(
      x,
      list(
        aka = NULL,
        tbl = NULL,
        exp = NULL,
        rex = NULL)
      ),
    switch(
      x$api,
      end = c(x$tbl),
      tmp = fmt_tmp(x$tbl)
      )
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

  x <- alias_lookup(alias)

  switch(
    x$clg,
    care = class_care(
      metadata   = get_metadata(x),
      dimensions = get_dimensions(x),
      identifier = switch(
        x$api,
        end = class_endpoint(x$identifier),
        tmp = class_temporal(slt(x$endpoints, -resources))),
      resources = switch(
        x$api,
        end = class_endpoint(x$resources),
        tmp = class_temporal(slt(x$endpoints, year, resources)))
    ),
    caid = class_caid(
      metadata   = get_metadata(x),
      dimensions = get_dimensions(x),
      identifier = switch(
        x$api,
        end = class_endpoint(x$identifier),
        tmp = class_temporal(x$endpoints)
      )
    ),
    prov = class_prov(
      metadata   = get_metadata(x),
      dimensions = get_dimensions(x),
      identifier = switch(
        x$api,
        end = class_endpoint(x$identifier),
        tmp = class_temporal(x$endpoints)
      )
    ),
    open = class_open(
      metadata   = get_metadata(x),
      dimensions = get_dimensions(x),
      identifier = switch(
        x$api,
        end = class_endpoint(x$identifier),
        tmp = class_temporal(x$endpoints)
      )
    ),
    hgov = class_hgov(
      metadata   = get_metadata(x),
      dimensions = get_dimensions(x),
      identifier = switch(
        x$api,
        end = class_endpoint(x$identifier),
        tmp = class_temporal(x$endpoints)
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
  check_required(alias)
  x <- group_regex(alias)

  class_collection(name = x$name,
                   members = names_map(x$alias, endpoint))
}

#' Create Groups of Endpoints
#'
#' @param alias `<chr>` Alias representing the CMS data endpoint.
#' @param desc `<chr>` Name of the group.
#' @returns S7 `class_group` object.
#' @examplesIf rlang::is_interactive()
#' group(c("cahps_hhc_patient", "hgov_qhp_business"))
#' group(c("hgov_local_help", "hgov_qhp_business"))
#' group(c("asc_facility", "care_dial_end"))
#' @autoglobal
#' @export
group <- function(alias, desc = NULL) {
  check_required(alias)
  class_group(
    name    = desc %||% paste0(alias, collapse = " | "),
    members = names_map(alias, endpoint)
  )
}
