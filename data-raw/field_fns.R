fields_current <- function(catalog_tbl, alias_list) {

  mirai::daemons(0)

  e <- rlang::enquo(catalog_tbl) |>
    rlang::as_label() |>
    strsplit("[$]", perl = TRUE) |>
    yank()

  x <- collapse::slt(catalog_tbl, title, modified, identifier) |>
    collapse::roworder(title, -modified)

  url_list <- rlang::set_names(x[["identifier"]], x[["title"]])

  # res <- base_url |>
  #   map(request) |>
  #   req_perform_parallel(on_error = "continue") |>
  #   map2(c("/dataset", rep(NA_character_, 4)), function(x, q) {
  #     resp_body_string(x) |>
  #       fparse(query = if (is.na(q)) NULL else q) |>
  #       as_fibble()
  #   }) |>
  #   set_names(names(base_url))

  mirai::daemons(6)

  res <- purrr::imap(url_list, purrr::in_parallel(\(x, i) {
    fastplyr::new_tbl(
      title = i,
      field = x |>
        httr2::request() |>
        httr2::req_error(is_error = ~ FALSE) |>
        httr2::req_perform() |>
        httr2::resp_body_json(simplifyVector = TRUE, check_type = FALSE) |>
        _$query |>
        _$properties
    )
  }))

  mirai::daemons(0)

  empty <- res |>
    purrr::keep(vctrs::vec_is_empty) |>
    names() |>
    fastplyr::f_enframe(value = "title")

  non_empty <- res |>
    purrr::discard(vctrs::vec_is_empty) |>
    purrr::list_rbind()

  vctrs::vec_rbind(non_empty, empty) |>
    collapse::mtt(catalog = e[3], point = e[4]) |>
    join_on_title(x) |>
    alias_column(alias_list) |>
    collapse::slt(catalog, point, alias, field, title, modified)
}

fields_current_care <- function(catalog_tbl, alias_list) {

  mirai::daemons(0)

  e <- rlang::enquo(catalog_tbl) |>
    rlang::as_label() |>
    strsplit("[$]", perl = TRUE) |>
    yank()

  x <- collapse::slt(catalog_tbl, title, modified, identifier) |>
    collapse::roworder(title, -modified)

  url_list <- rlang::set_names(x[["identifier"]], x[["title"]])

  mirai::daemons(6)

  res <- purrr::imap(url_list, purrr::in_parallel(\(x, i) {
    fastplyr::new_tbl(
      title = i,
      field = x |>
        httr2::request() |>
        httr2::req_error(is_error = ~ FALSE) |>
        httr2::req_perform() |>
        httr2::resp_body_json(simplifyVector = TRUE, check_type = FALSE) |>
        collapse::get_elem("meta") |>
        collapse::get_elem("headers")
    )
  }))

  mirai::daemons(0)

  empty <- res |>
    purrr::keep(vctrs::vec_is_empty) |>
    names() |>
    fastplyr::f_enframe(value = "title")

  non_empty <- res |>
    purrr::discard(vctrs::vec_is_empty) |>
    purrr::list_rbind()

  vctrs::vec_rbind(non_empty, empty) |>
    collapse::mtt(catalog = e[3], point = e[4]) |>
    join_on_title(x) |>
    alias_column(alias_list) |>
    collapse::slt(catalog, point, alias, field, title, modified) |>
    collapse::sbt(stringi::stri_detect_regex(title, "CMS Program Statistics", negate = TRUE))
}
