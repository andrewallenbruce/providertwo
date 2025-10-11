#' @autoglobal
#' @noRd
dim_current <- function(x) {
  i <- paste0(x$identifier, "?count=true&results=true&offset=0&limit=1") |>
    httr2::request() |>
    httr2::req_error(is_error = ~ FALSE) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE, check_type = FALSE)

  list(total = i$count, fields = collapse::get_elem(i, "properties"))
}

#' @autoglobal
#' @noRd
dim_care_current <- function(x) {
  i <- paste0(x$identifier, "?offset=0&size=1") |>
    httr2::request() |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE, check_type = FALSE) |>
    collapse::get_elem("meta") |>
    collapse::get_elem(c("total_rows", "headers"))

  list(total = i$total_rows, fields = i$headers)
}

#' @autoglobal
#' @noRd
dim_temporal <- function(x) {
  i <- paste0(x$identifier, "?count=true&results=true&offset=0&limit=1") |>
    map_perform_parallel() |>
    rlang::set_names(x$year)

  list(
    total = unname(purrr::map_int(i, function(x) x[["count"]])),
    fields = collapse::get_elem(i, "properties")
  )
}

#' @autoglobal
#' @noRd
dim_care_temporal <- function(x) {
  list(
    total = paste0(x$identifier, "/stats?offset=0&size=1") |>
      map_perform_parallel(query = "total_rows") |>
      unlist(use.names = FALSE),
    fields = paste0(x$identifier, "?count=true&results=true&offset=0&limit=1") |>
      map_perform_parallel(query = "names") |>
      rlang::set_names(x$year)
  )
}

#' @autoglobal
#' @noRd
reduce_fields <- function(x) {
  fastplyr::list_tidy(
    union = purrr::reduce(x, vctrs::vec_set_union) |>
      kit::psort(nThread = 4L),
    common = purrr::reduce(x, vctrs::vec_set_intersect) |>
      kit::psort(nThread = 4L),
    unique = purrr::imap(x, function(x, i)
      vctrs::vec_set_difference(x, y = f_common)) |>
      purrr::map(\(x) kit::psort(x, nThread = 4L))
  )
}
