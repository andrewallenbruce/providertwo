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
    tbl     = select_alias(glue("the$clog${catalog}${point}"), rex_endpoint(x)))

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
    care = switch(
      x$point,
      current        = class_care(
        access       = care_current(
          identifier = x$identifier,
          metadata   = get_meta(x),
          dimensions = get_dims(x)
        )
      ),
      temporal       = class_care(
        access       = care_temporal(
          identifier = x$endpoints,
          metadata   = get_meta(x),
          dimensions = get_dims(x)
        )
      )
    ),
    prov = class_prov(as_current(x)),
    caid = class_caid(switch(x$point, current = as_current(x),  temporal = as_temporal(x))),
    open = class_open(
      access = switch(
        x$point,
        current  = as_current(x),
        temporal = as_temporal(x))
    ),
    hgov = class_hgov(
      access = switch(
        x$point,
        current  = as_current(x),
        temporal = as_temporal(x))
    )
  )
}

#' @rdname load_endpoint
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

#' @rdname load_endpoint
#' @examples
#' group("asc_facility", "enterprise")
#' try(group("asc_facility"))
#' try(group("util"))
#' @autoglobal
#' @export
group <- function(..., .description = NULL) {

  check_dots_unnamed()
  alias <- purrr::compact(rlang::dots_list(..., .homonyms = "error"))

  if (any_are_collection(unlist(alias, use.names = FALSE))) {
    cli::cli_abort(
      c("x" = "A {.cls group} cannot contain a {.cls collection}.",
        "i" = "Run {.field collection({.val {glue::double_quote(alias[is_collection(alias)])}})} to load a collection."),
      call. = FALSE)
  }

  if (length(alias) == 1L) {
    cli::cli_abort(
      c("x" = "A {.cls group} contains more than one {.cls endpoint}.",
        "i" = "Run {.field endpoint({.val {glue::double_quote(alias)}})} to load an endpoint."),
      call. = FALSE)
  }

  class_group(
    name    = .description %||% paste0("[", paste0(alias, collapse = ", "), "]"),
    members = names_map(alias, endpoint)
  )
}
