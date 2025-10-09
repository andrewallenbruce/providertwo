#' Load Endpoints
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
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> endpoint aliases to be grouped
#' @param .title `<chr>` Group title. Defaults to `NULL`,
#'    which will concatenate the aliases for the title.
#' @name endpoint
#' @returns An S7 `<class_care/caid/prov/open/hgov/collection/group>` object.
NULL

#' @autoglobal
#' @noRd
select_alias <- function(x, alias) {
  collapse::sbt(eval(str2lang(x)), title %iin% alias_match_endpoint(alias))
}

# alias_lookup("dial_facility")
# alias_lookup("man_mltss")
# alias_lookup("ab_reg_comp")
# alias_lookup("asc_facility")
# alias_lookup("qppe")
#' @autoglobal
#' @noRd
alias_lookup <- function(x) {
  x <- list(alias   = x,
            point   = alias_endpoint_type(x),
            catalog = alias_catalog_name(x))

  lng <- glue::glue("the$clog${x$catalog}${x$point}")
  tbl <- select_alias(x = lng, alias = x$alias)

  check_alias_results(x$alias, tbl)

  cheapr::list_combine(x, switch(x$point, current = c(tbl), temporal = c2(tbl)))
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

#' @autoglobal
#' @noRd
as_current <- function(x) {
  i <- get_dims(x)

  class_current(
    identifier = x$identifier,
    metadata   = get_meta2(x),
    dimensions = i$dims,
    fields     = i$fields
  )
}

#' @autoglobal
#' @noRd
as_temporal <- function(x) {
  i <- get_dims(x)

  class_temporal(
    identifier = x$identifier,
    metadata   = get_meta2(x),
    dimensions = i$dims,
    fields     = i$fields,
    year       = x$year
  )
}

#' @autoglobal
#' @noRd
as_endpoint <- function(x) {
  switch(
    x$point,
    current  = as_current(x),
    temporal = as_temporal(x)
  )
}

#' @autoglobal
#' @noRd
as_care <- function(x) {
  i <- get_dims(x)

  class_care(
    access = switch(
      x$point,
      current = care_current(
        identifier = x$identifier,
        metadata   = get_meta2(x),
        dimensions = i$dims,
        fields     = i$fields,
        resources  = x$resources),
      temporal = care_temporal(
        identifier = x$identifier,
        metadata   = get_meta2(x),
        dimensions = i$dims,
        fields     = i$fields,
        resources  = x$resources,
        year       = x$year)
      )
    )
}

#' @rdname endpoint
#' @examples
#' endpoint("dial_facility")
#' endpoint("man_state")
#' try(endpoint(c("asc_facility", "enterprise")))
#' try(endpoint("unwind"))
#' try(endpoint("ex"))
#' @autoglobal
#' @export
endpoint <- function(alias) {

  check_required(alias)
  check_endpoint_alias(alias)

  x <- alias_lookup(alias)

  switch(
    x$catalog,
    care = as_care(x),
    prov = class_prov(as_current(x)),
    caid = class_caid(as_endpoint(x)),
    open = class_open(as_endpoint(x)),
    hgov = class_hgov(as_endpoint(x))
  )
}

#' @rdname endpoint
#' @examples
#' collection("unwind")
#' try(collection(c("asc_facility", "enterprise")))
#' try(collection("asc_facility"))
#' @autoglobal
#' @export
collection <- function(alias) {

  check_required(alias)
  check_collection_alias(alias)

  x <- alias_match_collection(alias)

  class_collection(
    title   = x$title,
    members = names_map(x$alias, endpoint))
}

#' @rdname endpoint
#' @examples
#' group("asc_facility", "enterprise")
#' try(group("asc_facility"))
#' try(group("util"))
#' @autoglobal
#' @export
group <- function(..., .title = NULL) {

  alias <- purrr::compact(rlang::dots_list(..., .homonyms = "error"))

  check_required(alias)
  check_group_alias(alias)

  class_group(
    title   = .title %||% theses(toString(alias)),
    members = names_map(alias, endpoint))
}
