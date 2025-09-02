#' @autoglobal
#' @noRd
get_dims <- function(x) {
  if (x$catalog == "care") {
    return(
      switch(
        x$point,
        current = dims_care_current(x),
        temporal = dims_care_temporal(x)))
  }

  i <- switch(
    x$point,
    current = paste0(x$identifier, "?count=true&results=true&offset=0&limit=1") |>
      httr2::request() |>
      httr2::req_error(is_error = ~ FALSE) |>
      perform_simple(),
    temporal = paste0(x$identifier, "?count=true&results=true&offset=0&limit=1") |>
      map_perform_parallel() |>
      rlang::set_names(x$year)
  )

  list(dims = class_dimensions(
    limit = api_limit(x$catalog),
    total = switch(
      x$point,
      current = i$count,
      temporal = unname(
        purrr::map_int(
          i, function(x) x[["count"]]
          )
        )
      )
    ),
  fields = class_fields(collapse::get_elem(i, "properties"))
  )
}

#' @autoglobal
#' @noRd
dims_care_temporal <- function(x) {
  list(
    dims = class_dimensions(
      limit = api_limit(x$catalog),
      total = paste0(
        x$identifier,
        "/stats?offset=0&size=1") |>
        map_perform_parallel(query = "total_rows") |>
        rlang::set_names(x$year) |>
        unlist(use.names = FALSE)
    ),
    fields = class_fields(
      keys = paste0(
        x$identifier,
        "?count=true&results=true&offset=0&limit=1") |>
        map_perform_parallel(query = "names") |>
        rlang::set_names(x$year)
    )
  )
}

#' @autoglobal
#' @noRd
dims_care_current <- function(x) {
  i <- paste0(x$identifier, "?offset=0&size=1") |>
    httr2::request() |>
    perform_simple() |>
    collapse::get_elem("meta") |>
    collapse::get_elem(c("total_rows", "headers"))

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
