#' @autoglobal
#' @noRd
alias_info <- function(x) {
  flist(
    aka = x,
    api = api_type(x),
    clg = catalog_type(x),
    rex = alias_regex(x),
    exp = str2lang(glue("the$catalog${clg}${api}"))
  )
}

#' @autoglobal
#' @noRd
fmt_tmp <- function(x) {
  # end <- yank(get_elem(res, "endpoints"))
  # if (all_na(gv(end, "resources"))) gv(end, "resources") <- NULL
  flist(
    !!!c(x[names(x) %!=% "endpoints"]),
    endpoints  = yank(x$endpoints),
    identifier = yank(endpoints$identifier)
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

  x <- alias_info(alias)
  i <- select_alias(eval(x$exp), x$rex)

  if (is_empty(i)) {
    cli_abort(c("x" = "{.field {x$aka}} ({.val {x$rex}}) had {nrow(i)} matches."))
  }

  if (nrow(i) > 1L) {
    cli_abort(c(
      "x" = "{.field {x$aka}} ({.val {x$rex}}) had {nrow(i)} matches:",
      cli::col_yellow(cli::format_bullets_raw(i$title))))
  }

  i <- switch(x$api, end = c(i), tmp = fmt_tmp(i))

  switch(
    x$clg,
    care = class_care(
      metadata   = get_metadata(i),
      dimensions = get_dimensions(i, x$clg, x$api),
      identifier = ifelse(
        x$api == "end",
        class_endpoint(i$identifier),
        class_temporal(slt(i$endpoints, -resources))
      ),
      resources  = ifelse(
        x$api == "end",
        class_endpoint(i$resources),
        class_temporal(slt(i$endpoints, year, resources))
      )
    ),
    caid = class_caid(
      metadata   = get_metadata(i),
      dimensions = get_dimensions(i, x$clg),
      identifier = ifelse(
        x$api == "end",
        class_endpoint(i$identifier),
        class_temporal(i$endpoints)
      )
    ),
    prov = class_prov(
      metadata   = get_metadata(i),
      dimensions = get_dimensions(i, x$clg),
      identifier = ifelse(
        x$api == "end",
        class_endpoint(i$identifier),
        class_temporal(i$endpoints)
      )
    ),
    open = class_open(
      metadata   = get_metadata(i),
      dimensions = get_dimensions(i, x$clg),
      identifier = ifelse(
        x$api == "end",
        class_endpoint(i$identifier),
        class_temporal(i$endpoints)
      )
    ),
    hgov = class_hgov(
      metadata   = get_metadata(i),
      dimensions = get_dimensions(i, x$clg),
      identifier = ifelse(
        x$api == "end",
        class_endpoint(i$identifier),
        class_temporal(i$endpoints)
      )
    )
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

  class_collection(name    = x$name,
                   members = names_map(x$alias, new_endpoint))
}

#' Create Groups of Endpoints
#'
#' @param alias `<chr>` Alias representing the CMS data endpoint.
#' @param desc `<chr>` Name of the group.
#' @returns S7 `class_group` object.
#' @examplesIf rlang::is_interactive()
#' new_group(c("cahps_hhc_patient", "hgov_qhp_business"))
#' new_group(c("hgov_local_help", "hgov_qhp_business"))
#' @autoglobal
#' @export
new_group <- function(alias, desc = NULL) {
  class_group(
    name    = desc %||% paste0(alias, collapse = " | "),
    members = names_map(alias, new_endpoint)
  )
}
