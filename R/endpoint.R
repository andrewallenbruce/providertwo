#' @autoglobal
#' @noRd
check_alias_results <- function(x, call = caller_env()) {
  if (is_empty(x$tbl)) {
    cli::cli_abort(
      c("x" = "{.field {x$aka}} ({.val {x$rex}}) had {nrow(x$tbl)} matches."),
      call = call)
  }

  if (nrow(x$tbl) > 1L) {
    cli::cli_abort(
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
    exp = glue("the$clog${clg}${pnt}"),
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

#' Load an endpoint by alias
#'
#' @param alias `<chr>` endpoint alias
#' @returns An S7 `<class_care/caid/prov/open/hgov>` object.
#' @examplesIf interactive()
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
    x$pnt,
    current = class_current(
      access = switch(
        x$clg,
        care = care_current(x$identifier, x$resources),
        caid = class_caid(x$identifier),
        prov = class_prov(x$identifier),
        open = class_open(x$identifier),
        hgov = class_hgov(x$identifier)
      ),
      metadata   = get_metadata(x),
      dimensions = get_dimensions(x)
    ),
    temporal = class_temporal(
      access = switch(
        x$clg,
        care = care_temporal(x$endpoints),
        caid = class_caid(x$endpoints),
        open = class_open(x$endpoints),
        hgov = class_hgov(x$endpoints)
      ),
      metadata   = get_metadata(x),
      dimensions = get_dimensions(x)
    )
  )

}

#' Load a collection of endpoints by alias
#'
#' @param alias `<chr>` collection alias
#' @returns An S7 `<class_collection>` object.
#' @examples
#' collection("caid_unwind")
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

#' Load a group of endpoints by alias
#'
#' @param alias `<chr>` Alias representing the CMS data endpoint.
#' @param description `<chr>` Group description. Defaults to `NULL`,
#'    which will use the aliases as the description.
#' @param call `<env>` Environment from which to call the function.
#' @returns S7 `<class_group>` object.
#' @examples
#' group(c("asc_facility", "care_dial_end"))
#' try(group("asc_facility"))
#' @autoglobal
#' @export
group <- function(alias, description = NULL, call = caller_env()) {

  check_required(alias)

  if (length(alias) == 1L) {
    cli::cli_abort(
      c("A {.cls class_group} must have more than one {.arg alias}.",
        "i" = "Run {.run endpoint({glue::double_quote(alias)})} to load this endpoint."),
      call = call)
  }

  class_group(
    name    = description %||% paste0(alias, collapse = " | "),
    members = names_map(alias, endpoint)
  )
}
