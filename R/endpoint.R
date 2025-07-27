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
  # slt(x$endpoints, -resources)
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
check_alias_results <- function(x, call = caller_env()) {
  if (is_empty(x$tbl)) {
    cli::cli_abort(
      c("x" = "{.field {x$alias}} ({.val {x$regex}}) had {nrow(x$tbl)} matches."),
      call = call)
  }

  if (nrow(x$tbl) > 1L) {
    cli::cli_abort(
      c("x" = "{.field {x$alias}} ({.val {x$regex}}) had {nrow(x$tbl)} matches:",
        cli::col_yellow(cli::format_bullets_raw(x$tbl$title))
      ),
      call = call
    )
  }

}

#' @param x `<chr>` endpoint alias
#' @returns `<list>` with elements:
#' @examplesIf interactive()
#' alias_lookup("dial_facility")
#' alias_lookup("man_mltss")
#' alias_lookup("ab_reg_comp")
#' alias_lookup("asc_facility")
#' alias_lookup("dial_listing")
#' @autoglobal
#' @noRd
alias_lookup <- function(x) {

  check_required(x)

  x <- flist(
    alias   = x,
    point   = point_type(x),
    catalog = catalog_type(x),
    tbl     = select_alias(
      glue("the$clog${catalog}${point}"),
      alias_rex(x)
      )
  )

  check_alias_results(x)

  list_combine(
    list_modify(x, list(tbl = NULL)),
    switch(
      x$point,
      current = c(x$tbl),
      temporal = c_temp(x$tbl)
      )
    )
}

#' Load an endpoint by alias
#'
#' @param alias `<chr>` endpoint alias
#' @returns An S7 `<class_care/caid/prov/open/hgov>` object.
#' @examples
#' endpoint("dial_facility")
#' endpoint("man_mltss")
#' endpoint("ab_reg_comp")
#' endpoint("asc_facility")
#' endpoint("dial_listing")
#' @autoglobal
#' @export
endpoint <- function(alias) {
  x <- alias_lookup(alias)

  switch(
    x$catalog,
    care = switch(
      x$point,
      current = class_care(
        access = care_current(
          identifier = x$identifier,
          metadata = get_metadata(x),
          dimensions = get_dimensions(x)
        )
      ),
      temporal = class_care(
        access = care_temporal(
          identifier = x$endpoints,
          metadata = get_metadata(x),
          dimensions = get_dimensions(x)
        )
      )
    ),
    prov = class_prov(
      access = class_current(
        identifier = x$identifier,
        metadata = get_metadata(x),
        dimensions = get_dimensions(x)
      )
    ),
    caid = switch(
      x$point,
      current = class_caid(
        access = class_current(
          identifier = x$identifier,
          metadata = get_metadata(x),
          dimensions = get_dimensions(x)
        )
      ),
      temporal = class_caid(
        access = class_temporal(
          identifier = x$endpoints,
          metadata = get_metadata(x),
          dimensions = get_dimensions(x)
        )
      )
    ),
    open = switch(
      x$point,
      current = class_open(
        access = class_current(
          identifier = x$identifier,
          metadata = get_metadata(x),
          dimensions = get_dimensions(x)
        )
      ),
      temporal = class_open(
        access = class_temporal(
          identifier = x$endpoints,
          metadata = get_metadata(x),
          dimensions = get_dimensions(x)
        )
      )
    ),
    hgov = switch(
      x$point,
      current = class_hgov(
        access = class_current(
          identifier = x$identifier,
          metadata = get_metadata(x),
          dimensions = get_dimensions(x)
        )
      ),
      temporal = class_hgov(
        access = class_temporal(
          identifier = x$endpoints,
          metadata = get_metadata(x),
          dimensions = get_dimensions(x)
        )
      )
    )
  )

}

#' Load a collection of endpoints by alias
#'
#' @param alias `<chr>` collection alias
#' @returns An S7 `<class_collection>` object.
#' @examples
#' collection("unwind")
#' collection("managed")
#' @autoglobal
#' @export
collection <- function(alias) {

  check_required(alias)

  x <- rex_collect(alias)

  class_collection(
    name    = x$name,
    members = names_map(x$alias, endpoint))
}

#' Load a group of endpoints by alias
#'
#' @param alias `<chr>` Alias representing the CMS data endpoint.
#' @param description `<chr>` Group description. Defaults to `NULL`,
#'    which will use the aliases as the description.
#' @param call `<env>` Environment from which to call the function.
#' @returns S7 `<class_group>` object.
#' @examples
#' group(c("asc_facility", "enterprise"))
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
    name    = description %||% paste0("[", paste0(alias, collapse = ", "), "]"),
    members = names_map(alias, endpoint)
  )
}
