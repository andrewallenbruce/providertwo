#' @autoglobal
#' @noRd
select_alias <- function(alias, catalog, point) {

  x <- str2lang(glue::glue("the$clog${catalog}${point}"))

  collapse::sbt(eval(x), title %iin% alias_match_endpoint(alias))
}

#' @autoglobal
#' @noRd
c1 <- function(x, tbl) {
  switch(x,
    current  = cheapr::na_rm(c(tbl)),
    temporal = c2(tbl))
}

#' @autoglobal
#' @noRd
c2 <- function(x) {
  end <- yank(x$endpoints) |>
    collapse::sbt(!is.na(identifier)) |>
    collapse::gvr(c("^year$", "^identifier$", "^download$", "^resources$"))

  mod <- collapse::fmax(yank(x$endpoints, "modified"))

  if (collapse::allNA(end$download)) collapse::gv(end, "download") <- NULL

  cheapr::list_combine(collapse::char_vars(x), modified = mod, end)
}

# alias_lookup("dial_facility")
# alias_lookup("man_mltss")
# alias_lookup("ab_reg_comp")
# alias_lookup("asc_facility")
# alias_lookup("qppe")
#' @autoglobal
#' @noRd
alias_lookup <- function(x, call = rlang::caller_env()) {

  check_endpoint_alias(x, call = call)

  x <- fastplyr::list_tidy(
    alias   = x,
    point   = alias_endpoint_type(x),
    catalog = alias_catalog_name(x),
    limit   = api_limit(catalog))

  tbl <- select_alias(x$alias, x$catalog, x$point)

  check_alias_results(x$alias, tbl, call = call)

  x <- cheapr::list_combine(x, c1(x$point, tbl))
  x <- cheapr::list_combine(x, get_dimensions(x))

  cheapr::list_modify(x,
    list(
      fields = switch(
        x$point,
        current = x$fields,
        temporal = fields_df(x$fields)),
      dimensions = class_dimensions(x$limit, x$total)
      ))
}
