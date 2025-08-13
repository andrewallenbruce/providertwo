#' @autoglobal
#' @noRd
c2 <- function(x) {
  end <- gv(yank(x$endpoints), c("year", "identifier", "download", "resources"))
  flist(!!!c(x[names(x) %!=% "endpoints"]), !!!c(end))
}

# alias_lookup2("dial_facility")
# alias_lookup2("man_mltss")
# alias_lookup2("ab_reg_comp")
# alias_lookup2("asc_facility")
# alias_lookup2("qppe")
#' @autoglobal
#' @noRd
alias_lookup2 <- function(x) {

  x <- flist(
    alias   = x,
    point   = point_type(x),
    catalog = catalog_type(x),
    tbl     = select_alias(glue("the$clog${catalog}${point}"), x))

  check_alias_results(x)

  list_combine(
    list_modify(x, list(tbl = NULL)),
    switch(
      x$point,
      current  = c(x$tbl),
      temporal = c2(x$tbl)))
}

#' @autoglobal
#' @noRd
get_dims2 <- function(x) {

  utypes <- map_chr(x$identifier, url_type)

  # if (utype %in% c("current", "temporal")) return(dims_care(x))

  x <- switch(
    url_type(x$identifier),
    current = dims_care_current(x),
    temporal = dims_care_temporal(x)
  )

  id <- paste0(x$identifier, "?count=true&results=true&offset=0&limit=1") |>
    request() |>
    req_error(is_error = ~ FALSE) |>
    perform_simple()


  # care
  list(
    dims = class_dimensions(limit = api_limit("care"), total = x$total_rows %||% 0L),
    fields = class_fields(x$headers)
  )
  # default
  list(
    dims = class_dimensions(
      limit = api_limit(utype),
      total = id$count %||% 0L),
    fields = class_fields(id$query$properties)
  )
}

#' @autoglobal
#' @noRd
dims_care_temporal <- function(x) {
  list(
    headers = paste0(x$identifier, "?offset=0&size=1") |>
      map(request) |>
      req_perform_parallel(on_error = "continue") |>
      map(function(x)
        names2(parse_string(x))) |>
      set_names(x$year),
    total_rows = paste0(x$identifier, "/stats?offset=0&size=1") |>
      map(request) |>
      req_perform_parallel(on_error = "continue") |>
      map(function(x)
        parse_string(x) |> get_elem("total_rows")) |>
      set_names(x$year)
  )
}

#' @autoglobal
#' @noRd
dims_care_current <- function(x) {

  paste0(x$identifier, "?offset=0&size=1") |>
    request() |>
    perform_simple() |>
    get_elem("meta") |>
    get_elem(c("total_rows", "headers"))
}

#' @autoglobal
#' @noRd
as_temporal <- function(x) {
  i <- get_dims(x)

  class_temporal(
    identifier = x$endpoints,
    metadata   = get_meta(x),
    dimensions = i$dims,
    fields     = i$fields
  )
}
