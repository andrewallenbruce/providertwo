fields_current <- function(catalog_tbl, alias_list) {

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

fields_temporal_care <- function(catalog_tbl, alias_list) {

  mirai::daemons(0)

  e <- rlang::enquo(catalog_tbl) |>
    rlang::as_label() |>
    strsplit("[$]", perl = TRUE) |>
    yank()

  x <- tidyr::unnest(catalog_tbl, endpoints) |>
    collapse::slt(title, year, modified, identifier) |>
    collapse::roworder(title, -year)

  url_list <- rlang::set_names(
    x[["identifier"]],
    paste0(x[["year"]], "|", x[["title"]]))

  mirai::daemons(6)

  res <- purrr::imap(url_list, purrr::in_parallel(\(x, i) {
    cheapr::new_df(
      title = i,
      field = x |>
        httr2::request() |>
        httr2::req_error(is_error = ~ FALSE) |>
        httr2::req_perform() |>
        httr2::resp_body_json(
          simplifyVector = TRUE,
          check_type = FALSE) |>
        rlang::names2()
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
    tidyr::separate_wider_delim(title, delim = "|", names = c("year", "title")) |>
    collapse::mtt(year = as.integer(year), title = rm_nonascii(title)) |>
    join_on_title(x) |>
    alias_column(alias_list) |>
    collapse::slt(catalog, point, alias, field, title, year) |>
    funique() |>
    fastplyr::as_tbl()
}

fields_temporal <- function(catalog_tbl, alias_list) {

  mirai::daemons(0)

  e <- rlang::enquo(catalog_tbl) |>
    rlang::as_label() |>
    strsplit("[$]", perl = TRUE) |>
    yank()

  x <- tidyr::unnest(catalog_tbl, endpoints) |>
    collapse::slt(title, year, modified, identifier) |>
    collapse::roworder(title, -year)

  url_list <- rlang::set_names(
    paste0(
      x[["identifier"]],
      "?count=true&results=true&offset=0&limit=1"),
    paste0(x[["year"]], "|", x[["title"]]))

  mirai::daemons(6)

  res <- purrr::imap(url_list, purrr::in_parallel(\(x, i) {
    cheapr::new_df(
      title = i,
      field = x |>
        httr2::request() |>
        httr2::req_error(is_error = ~ FALSE) |>
        httr2::req_perform() |>
        httr2::resp_body_json(
          simplifyVector = TRUE,
          check_type = FALSE) |>
        collapse::get_elem("properties")
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
    tidyr::separate_wider_delim(title, delim = "|", names = c("year", "title")) |>
    collapse::mtt(year = as.integer(year), title = rm_nonascii(title)) |>
    join_on(x, on = c("title")) |>
    alias_column(alias_list) |>
    collapse::slt(catalog, point, alias, field, title, year) |>
    funique() |>
    fastplyr::as_tbl()
}
