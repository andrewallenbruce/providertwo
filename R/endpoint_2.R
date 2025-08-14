#' @autoglobal
#' @noRd
c2 <- function(x) {

  cols <- c("year", "identifier", "download")

  end <- yank(x$endpoints)

  end <- `if`(
    "resources" %in_% names2(end),
    gv(end, c(cols, "resources")),
    gv(end, cols))

  if (collapse::allNA(end$download)) gv(end, "download") <- NULL

  flist(!!!c(x[names(x) %!=% "endpoints"]), !!!c(end))
}

# alias_lookup2("dial_facility")
# alias_lookup2("man_mltss")
# alias_lookup2("ab_reg_comp")
# alias_lookup2("asc_facility")
# alias_lookup2("qppe")
#' @autoglobal
#' @noRd
alias_lookup <- function(x) {
  x <- flist(
    alias   = x,
    point   = point_type(x),
    catalog = catalog_type(x),
    tbl     = select_alias(glue("the$clog${catalog}${point}"), x)
  )

  check_alias_results(x)

  list_combine(list_modify(x, list(tbl = NULL)), switch(
    x$point,
    current  = c(x$tbl),
    temporal = c2(x$tbl)
  ))
}

#' @autoglobal
#' @noRd
get_dims <- function(x) {

  if (x$catalog == "care") {
    return(
      switch(
        x$point,
        current = dims_care_current(x),
        temporal = dims_care_temporal(x)
        )
      )
  }


  i <- switch(
    x$point,
    current = paste0(x$identifier, "?count=true&results=true&offset=0&limit=1") |>
      request() |>
      req_error(is_error = ~ FALSE) |>
      perform_simple(),
    temporal = paste0(x$identifier, "?count=true&results=true&offset=0&limit=1") |>
      map(request) |>
      req_perform_parallel(on_error = "continue") |>
      resps_successes() |>
      map(function(x)
        parse_string(x)) |>
      set_names(x$year)
  )

  list(
    dims = class_dimensions(
      limit = api_limit(x$catalog),
      total = switch(
        x$point,
        current = i$count,
        temporal = unname(map_int(i, \(x) x[["count"]]))
        )
      ),
    fields = class_fields(get_elem(i, "properties"))
    )
}

#' @autoglobal
#' @noRd
dims_care_temporal <- function(x) {
  list(
    dims = class_dimensions(
      limit = api_limit(x$catalog),
      total = paste0(x$identifier, "/stats?offset=0&size=1") |>
        map(request) |>
        req_perform_parallel(on_error = "continue") |>
        resps_successes() |>
        map(function(x)
          parse_string(x) |>
            get_elem("total_rows")) |>
        set_names(x$year) |>
        unlist(use.names = FALSE)
      ),
    fields = class_fields(
      keys = paste0(x$identifier, "?offset=0&size=1") |>
        map(request) |>
        req_perform_parallel(on_error = "continue") |>
        resps_successes() |>
        map(function(x)
          names2(parse_string(x))) |>
        set_names(x$year)
      )
    )
}

#' @autoglobal
#' @noRd
dims_care_current <- function(x) {
  i <- paste0(x$identifier, "?offset=0&size=1") |>
    request() |>
    perform_simple() |>
    get_elem("meta") |>
    get_elem(c("total_rows", "headers"))

  list(
    dims = class_dimensions(
      limit = api_limit(x$catalog),
      total = i$total_rows),
    fields = class_fields(i$headers)
  )
}

#' @autoglobal
#' @noRd
reduce_fields <- function(x) {
  flist(
    f_union = purrr::reduce(x$headers, vctrs::vec_set_union) |>
      kit::psort(nThread = 4L),
    f_common = purrr::reduce(x$headers, vctrs::vec_set_intersect) |>
      kit::psort(nThread = 4L),
    f_unique = purrr::imap(x$headers, function(x, i)
      vctrs::vec_set_difference(x, y = f_common)) |>
      purrr::map(\(x) kit::psort(x, nThread = 4L))
  )
}
