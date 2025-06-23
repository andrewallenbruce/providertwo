#' @autoglobal
#' @noRd
catalog_type <- function(x) {

  nif(
    !is.na(mph_match(x, type$care)), "care",
    !is.na(mph_match(x, type$caid)), "caid",
    !is.na(mph_match(x, type$prov)), "prov",
    !is.na(mph_match(x, type$open)), "open",
    !is.na(mph_match(x, type$hgov)), "hgov",
    default = NA_character_)

}

#' @autoglobal
#' @noRd
api_type <- function(x) {

  nif(
    !is.na(mph_match(x, type$endpoint)),   "end",
    !is.na(mph_match(x, type$temporal)),   "tmp",
    # !is.na(mph_match(x, type$collection)), "class_collection",
    default = NA_character_)

}

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

#' Medicare API Endpoints
#' @name medicare
#' @param alias `<chr>` endpoint alias
#' @returns An S7 `<class_care>` object
#' @examplesIf rlang::is_interactive()
#' new_endpoint("care_dialysis")
#' new_endpoint("quality_payment")
NULL

#' @autoglobal
#' @rdname medicare
#' @export
new_endpoint <- function(alias) {

  x   <- alias_info(alias)

  res <- select_alias(eval(x$str), x$regex)

  if (is_empty(res))  cli_abort(c("x" = "{.val {x$regex}} has 0 matches."))
  if (nrow(res) > 1L) cli_abort(c("x" = "{.val {x$regex}} has multiple matches."))

  a <- switch(
    x$api,
    end = c(res),
    tmp = flist(
    !!!c(slt(res, -endpoints)),
    endpoints = pluck(get_elem(res, "endpoints"), 1),
    identifier = pluck(get_elem(endpoints, "identifier"), 1))
  )

  switch(
    x$catalog,
    care = class_care(metadata   = get_metadata(a),
                      dimensions = get_dimensions(a, x$catalog, x$api),
                      identifier = if (x$api == "end") class_endpoint(a$identifier) else class_temporal(slt(a$endpoints, -resources)),
                      resources  = if (x$api == "end") class_endpoint(a$resources) else class_temporal(slt(a$endpoints, year, resources))),
    caid = class_caid(metadata   = get_metadata(a),
                      dimensions = get_dimensions(a, x$catalog, x$api),
                      identifier = if (x$api == "end") class_endpoint(a$identifier) else class_temporal(a$endpoints)),
    prov = class_prov(metadata   = get_metadata(a),
                      dimensions = get_dimensions(a, x$catalog, x$api),
                      identifier = if (x$api == "end") class_endpoint(a$identifier) else class_temporal(a$endpoints)),
    open = class_open(metadata   = get_metadata(a),
                      dimensions = get_dimensions(a, x$catalog, x$api),
                      identifier = if (x$api == "end") class_endpoint(a$identifier) else class_temporal(a$endpoints)),
    hgov = class_hgov(metadata   = get_metadata(a),
                      dimensions = get_dimensions(a, x$catalog, x$api),
                      identifier = if (x$api == "end") class_endpoint(a$identifier) else class_temporal(a$endpoints)),
  )
}

