#' Load Data Endpoints
#'
#' @description
#' Load a data endpoint or collection by its alias.
#'
#' @details
#' The `endpoint()` function loads a single data endpoint, while the
#' `collection()` function loads a collection of endpoints, and the `group()`
#' function loads a group of endpoints. The `alias` parameter is used to specify
#' the endpoint or collection alias. If the alias is not found, an error will be
#' raised.
#'
#' @param alias `<chr>` endpoint or collection alias
#' @inheritParams rlang::args_dots_used
#' @param .description `<chr>` Group description. Defaults to `NULL`,
#'    which will use the aliases as the description.
#' @name load_endpoint
#' @returns An S7 `<class_care/caid/prov/open/hgov/collection/group>` object.
NULL

#' @autoglobal
#' @noRd
select_alias <- function(x, alias, ...) {
  subset_detect(i = eval(str2lang(x)), j = title, p = alias, ...)
}

#' @autoglobal
#' @noRd
c_temp <- function(x) {
  flist(
    !!!c(x[names(x) %!=% "endpoints"]),
    endpoints  = yank(x$endpoints),
    identifier = yank(endpoints$identifier))
}

# x <- list(
# alias = "asfsadgff",
# regex = "13526#%&#%%&",
# tbl = fastplyr::new_tbl(title = c("test1", "test2")))
#' @autoglobal
#' @noRd
check_alias_results <- function(x, call = caller_env()) {
  msg <- c("x" = "{.field {x$alias}} ({.val {x$regex}}) had {nrow(x$tbl)} matches.")

  if (is_empty(x$tbl)) {
    cli::cli_abort(msg, call = call)
  }

  if (nrow(x$tbl) > 1L) {
    msg <- c(msg, cli::col_yellow(cli::format_bullets_raw(x$tbl$title)))
    cli::cli_abort(msg, call = call)
  }

}

# alias_lookup("dial_facility")
# alias_lookup("man_mltss")
# alias_lookup("ab_reg_comp")
# alias_lookup("asc_facility")
# alias_lookup("dial_listing")
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
      rex_endpoint(x)))

  check_alias_results(x)

  list_combine(
    list_modify(x, list(tbl = NULL)),
    switch(
      x$point,
      current = c(x$tbl),
      temporal = c_temp(x$tbl)))
}

#' @autoglobal
#' @noRd
as_current <- function(x) {
  class_current(
    identifier   = x$identifier,
    metadata     = get_meta(x),
    dimensions   = get_dims(x)
  )
}

#' @autoglobal
#' @noRd
as_temporal <- function(x) {
  class_temporal(
    identifier = x$endpoints,
    metadata   = get_meta(x),
    dimensions = get_dims(x)
  )
}

#' @autoglobal
#' @noRd
as_point <- function(x) {
  switch(
    x$point,
    current  = as_current(x),
    temporal = as_temporal(x)
  )
}

#' @autoglobal
#' @noRd
as_care <- function(x) {
  class_care(switch(
    x$point,
    current      = care_current(
      identifier = x$identifier,
      metadata   = get_meta(x),
      dimensions = get_dims(x)
    ),
    temporal     = care_temporal(
      identifier = x$endpoints,
      metadata   = get_meta(x),
      dimensions = get_dims(x)
    )
  ))
}

#' @rdname load_endpoint
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
    care = as_care(x),
    prov = class_prov(as_current(x)),
    caid = class_caid(as_point(x)),
    open = class_open(as_point(x)),
    hgov = class_hgov(as_point(x))
  )
}

#' @rdname load_endpoint
#' @examples
#' collection("unwind")
#' collection("managed")
#' try(collection(c("asc_facility", "enterprise")))
#' try(collection("asc_facility"))
#' @autoglobal
#' @export
collection <- function(alias) {

  check_required(alias)

  if (length(alias) > 1L) {
    cli::cli_abort(
      c("x" = "Only one {.cls collection} can be selected at a time."),
      call. = FALSE)
  }

  if (!is_collection(alias)) {
    cli::cli_abort(
      c("x" = "{.val {alias}} is not a {.cls collection} {.field alias}."),
      call. = FALSE)
  }

  x <- rex_collect(alias)

  class_collection(
    name    = x$name,
    members = names_map(x$alias, endpoint))
}

#' @rdname load_endpoint
#' @examples
#' group("asc_facility", "enterprise")
#' try(group("asc_facility"))
#' try(group("util"))
#' @autoglobal
#' @export
group <- function(..., .description = NULL) {

  alias <- purrr::compact(rlang::dots_list(..., .homonyms = "error"))
  avec  <- unlist(alias, use.names = FALSE)

  if (rlang::is_empty(alias) || any(!nzchar(alias))) {
    cli::cli_abort(
      c("x" = "A {.cls group} must contain at least two {.cls endpoints}."),
      call. = FALSE)
  }

  if (any_are_collection(avec)) {
    cli::cli_abort(
      c("x" = "A {.cls group} cannot contain a {.cls collection}.",
        ">" = "Run e.g., {.field collection({.val {avec[is_collection(avec)][[1]]}})}."),
      call. = FALSE)
  }

  if (length(avec) == 1L) {
    cli::cli_abort(
      c("x" = "A {.cls group} must contain at least two {.cls endpoints}.",
        ">" = "Run e.g., {.field endpoint({.val {avec}})} to load an endpoint."),
      call. = FALSE)
  }

  class_group(
    name    = .description %||% brackets(toString(alias)),
    members = names_map(alias, endpoint)
  )
}
