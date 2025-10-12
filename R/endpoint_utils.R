#' @autoglobal
#' @noRd
select_alias <- function(alias, catalog, point) {

  x <- str2lang(glue::glue("the$clog${catalog}${point}"))

  collapse::sbt(
    eval(x), title %iin% alias_match_endpoint(alias))
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

  x <- cheapr::list_combine(x, switch(x$point, current = c(tbl), temporal = c2(tbl)))
  x <- cheapr::list_combine(x, dimensions(x))
  x$fields     <- class_fields(x$fields)
  x$dimensions <- class_dimensions(x$limit, x$total)
  x$limit      <- NULL
  x$total      <- NULL
  x
}

# endpoint2("dial_facility")
# endpoint2("man_mltss")
# endpoint2("ab_reg_comp")
# endpoint2("asc_facility")
# endpoint2("qppe")
#' @autoglobal
#' @noRd
endpoint2 <- function(alias, call = rlang::caller_env()) {

  check_required(alias, call = call)
  check_endpoint_alias(alias, call = call)

  x <- alias_lookup(alias, call = call)

  x$point <- switch(
    x$catalog,
    care = glue::glue("care_{x$point}"),
    glue::glue("class_{x$point}"))

  .pnt <- rlang::as_function(
    x$point,
    env = rlang::pkg_env("providertwo"))

  x$catalog <- glue::glue("class_{x$catalog}")

  .cls <- rlang::as_function(
    x$catalog,
    env = rlang::pkg_env("providertwo"))

  x$fields     <- class_fields(x$fields)
  x$dimensions <- class_dimensions(x$limit, x$total)
  x$limit      <- NULL
  x$total      <- NULL

  x <- x[names(.pnt@properties)]

  arg_nms <- names(x)

  list2(!!!x)

  .cls(
    .pnt(!!!rlang::dots_list(x))
    )
}
