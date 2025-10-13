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
#' @param call The environment from which the function is called.
#' @name endpoint
#' @returns An S7 `<endpoint/collection/group>` object.
NULL

#' @autoglobal
#' @noRd
as_current <- function(x) {
  class_current(
    identifier = x$identifier,
    alias      = x$alias,
    title      = x$title,
    modified   = x$modified,
    dimensions = x$dimensions,
    fields     = x$fields
  )
}

#' @autoglobal
#' @noRd
as_temporal <- function(x) {
  class_temporal(
    identifier = x$identifier,
    alias      = x$alias,
    title      = x$title,
    modified   = x$modified,
    dimensions = x$dimensions,
    fields     = x$fields,
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
  class_care(
    access = switch(
      x$point,
      current = care_current(
        identifier = x$identifier,
        alias      = x$alias,
        title      = x$title,
        modified   = x$modified,
        dimensions = x$dimensions,
        fields     = x$fields,
        resources  = x$resources),
      temporal = care_temporal(
        identifier = x$identifier,
        alias      = x$alias,
        title      = x$title,
        modified   = x$modified,
        dimensions = x$dimensions,
        fields     = x$fields,
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
endpoint <- function(alias, call = rlang::caller_env()) {

  check_required(alias, call = call)
  check_endpoint_alias(alias, call = call)

  x <- alias_lookup(alias, call = call)

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
collection <- function(alias, call = rlang::caller_env()) {

  check_required(alias, call = call)
  check_collection_alias(alias, call = call)

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
group <- function(..., .title = NULL, call = rlang::caller_env()) {

  alias <- purrr::compact(rlang::dots_list(..., .homonyms = "error"))

  check_required(alias, call = call)
  check_group_alias(alias, call = call)

  class_group(
    title   = .title %||% theses(toString(alias)),
    members = names_map(alias, endpoint))
}
