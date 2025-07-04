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
c_temp <- function(x) {
  # if (all_na(gv(end, "resources"))) gv(end, "resources") <- NULL
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
    pnt = point_type(x),
    clg = clog_type(x),
    rex = alias_rex(x),
    exp = glue("the$catalog${clg}${pnt}"),
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
        rex = NULL
        )
      ),
    switch(
      x$pnt,
      current = c(x$tbl),
      temporal = c_temp(x$tbl)
      )
    )
}

#' Load API Endpoint
#' @param alias `<chr>` endpoint alias
#' @returns S7 `<class_[Catalog Name]>` object.
#' @examples
#' endpoint("care_dial_end")
#' endpoint("care_dial_tmp")
#' endpoint("managed_mltss")
#' endpoint("hgov_ab_reg_comp")
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
      identifier = get_identifier(x),
      resources  = get_resources(x)
    ),
    caid = class_caid(
      metadata   = get_metadata(x),
      dimensions = get_dimensions(x),
      identifier = get_identifier(x)
    ),
    prov = class_prov(
      metadata   = get_metadata(x),
      dimensions = get_dimensions(x),
      identifier = get_identifier(x)
    ),
    open = class_open(
      metadata   = get_metadata(x),
      dimensions = get_dimensions(x),
      identifier = get_identifier(x)
    ),
    hgov = class_hgov(
      metadata   = get_metadata(x),
      dimensions = get_dimensions(x),
      identifier = get_identifier(x)
    )
  )
}

#' Load Collection of Endpoints
#' @param alias `<chr>` Alias representing the CMS data endpoint.
#' @returns S7 `<class_collection>` object.
#' @examplesIf rlang::is_interactive()
#' collection("caid_demographics")
#' collection("caid_unwind")
#' collection("caid_services")
#' @autoglobal
#' @export
collection <- function(alias) {

  check_required(alias)

  x <- collect_rex(alias)

  class_collection(
    name = x$name,
    members = names_map(x$alias, endpoint)
    )
}

#' Create Group of Endpoints
#' @param alias `<chr>` Alias representing the CMS data endpoint.
#' @param description `<chr>` Group description. Defaults to `NULL`,
#'    which will use the aliases as the description.
#' @returns S7 `<class_group>` object.
#' @examplesIf rlang::is_interactive()
#' group(c("cahps_hhc_patient", "hgov_qhp_business"))
#' group(c("hgov_local_help", "hgov_qhp_business"))
#' group(c("asc_facility", "care_dial_end"))
#' @autoglobal
#' @export
group <- function(alias, description = NULL) {

  check_required(alias)

  class_group(
    name    = description %||% paste0(alias, collapse = " | "),
    members = names_map(alias, endpoint)
  )
}
