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
#' @name load_endpoint
#' @returns An S7 `<class_care/caid/prov/open/hgov/collection/group>` object.
NULL

#' @autoglobal
#' @noRd
select_alias <- function(s, alias, ...) {
  subset_detect(i = eval(str2lang(s)), j = title, p = rex_endpoint(alias), ...)
}

#' @autoglobal
#' @noRd
c2 <- function(x) {

  cols <- c("year", "identifier", "download")

  end <- yank(x$endpoints) |> collapse::sbt(!is.na(identifier))

  end <- if ("resources" %in_% rlang::names2(end)) {
    collapse::gv(end, c(cols, "resources"))
  } else {
    collapse::gv(end, cols)
  }

  if (collapse::allNA(end$download)) collapse::gv(end, "download") <- NULL

  flist(
    !!!c(x[names(x) %!=% "endpoints"]),
    modified = max(yank(x$endpoints)$modified),
    !!!c(end))
}

# alias_lookup("dial_facility")
# alias_lookup("man_mltss")
# alias_lookup("ab_reg_comp")
# alias_lookup("asc_facility")
# alias_lookup("qppe")
#' @autoglobal
#' @noRd
alias_lookup <- function(x) {
  x <- flist(
    alias   = x,
    point   = point_type(x),
    catalog = catalog_type(x),
    tbl     = select_alias(glue::glue("the$clog${catalog}${point}"), x)
  )

  check_alias_results(x)

  cheapr::list_combine(
    cheapr::list_modify(x, list(tbl = NULL)),
    switch(
      x$point,
      current  = c(x$tbl),
      temporal = c2(x$tbl)
      )
    )
}

#' @autoglobal
#' @noRd
as_current <- function(x) {
  i <- get_dims(x)

  class_current(
    identifier = x$identifier,
    metadata   = get_meta(x),
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
    metadata   = get_meta(x),
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
      current      = care_current(
        identifier = x$identifier,
        metadata   = get_meta(x),
        dimensions = i$dims,
        fields     = i$fields),
      temporal     = care_temporal(
        identifier = x$identifier,
        metadata   = get_meta(x),
        dimensions = i$dims,
        fields     = i$fields,
        year       = x$year)
      )
    )
}

#' @rdname load_endpoint
#' @examples
#' endpoint("dial_facility")
#' endpoint("man_mltss")
#' @autoglobal
#' @export
endpoint <- function(alias) {
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

#' @rdname load_endpoint
#' @examples
#' collection("unwind")
#' try(collection(c("asc_facility", "enterprise")))
#' try(collection("asc_facility"))
#' @autoglobal
#' @export
collection <- function(alias) {

  check_required(alias)
  check_collection(alias)

  x <- rex_collect(alias)

  class_collection(
    title   = x$name,
    members = names_map(x$alias, endpoint))
}

#' @rdname load_endpoint
#' @examples
#' group("asc_facility", "enterprise")
#' try(group("asc_facility"))
#' try(group("util"))
#' @autoglobal
#' @export
group <- function(..., .title = NULL) {

  alias <- purrr::compact(rlang::dots_list(..., .homonyms = "error"))

  check_group(alias)

  class_group(
    title   = .title %||% theses(toString(alias)),
    members = names_map(alias, endpoint))
}
