source(here::here("data-raw", "pins_internal.R"))

unlist_elem <- function(x, name, ...) {
  collapse::get_elem(x, elem = name, ...) |>
    unlist(use.names = FALSE)
}

open_dictionary <- function() {

  x <- RcppSimdJson::fload(
    paste0(
      "https://openpaymentsdata.cms.gov/api/1/",
      "metastore/schemas/dataset/items?show-reference-ids"
      )
    ) |>
    collapse::get_elem("data", DF.as.list = TRUE) |>
    collapse::get_elem("title|describedBy$", regex = TRUE) |>
    purrr::map(\(x) x[!is.null(names(x))])

  cheapr::new_df(
    name     = unlist_elem(x, "title"),
    download = unlist_elem(x, "describedBy")) |>
    collapse::mtt(
      year = extract_year(name),
      name = ifelse(
        is.na(year),
        name,
        stringi::stri_extract_all_regex(
          name,
          "^.*(?=\\s.\\sDetailed Dataset [0-9]{4} Reporting Year)"
        )
      ),
      year = ifelse(is.na(year), collapse::fmax(year), year)
    ) |>
    collapse::sbt(year == collapse::fmax(year), -year) |>
    collapse::get_elem("download") |>
    purrr::map(request) |>
    httr2::req_perform_parallel(on_error = "continue") |>
    httr2::resps_successes() |>
    purrr::map(
      function(resp)
        parse_string(resp, query = "/data") |>
        collapse::get_elem("fields") |>
        map_na_if() |>
        cheapr::as_df() |>
        collapse::mtt(
          description = rm_nonascii(description),
          description = gremove(description, "[\n\"']"),
          description = greplace(description, "[\\\\]", "-"),
          description = greplace(description, "\\s+", " ")
        )
    ) |>
    rlang::set_names(
      collapse::get_elem(x, "title") |>
        stringi::stri_extract_all_regex(
          paste0(
            "^.*(?=\\s.\\sDetailed Dataset ",
            "[0-9]{4} Reporting Year)",
            "|Covered Recipient Profile Supplement"
          )
        ) |>
        unlist(use.names = FALSE) |>
        collapse::funique()
    ) |>
    purrr::list_rbind(names_to = "endpoint") |>
    collapse::slt(endpoint, field = title, description, type, fmt = format, enum = constraints) |>
    collapse::mtt(
      enum = purrr::map(enum, \(x) toString(x)) |> unlist(use.names = FALSE),
      enum = ifelse(startsWith(enum, "c"), enum, NA_character_),
      enum = rm_quotes(enum),
      enum = gremove(enum, "c[(]|[)]"),
      catalog = "open") |>
    fastplyr::as_tbl()
}

open <- open_dictionary()
open


caid_dictionary <- function() {
  x <- RcppSimdJson::fload(
    "https://data.medicaid.gov/api/1/metastore/schemas/dataset/items?show-reference-ids"
  ) |>
    collapse::get_elem("distribution") |>
    collapse::rowbind(fill = TRUE) |>
    collapse::get_elem("data") |>
    collapse::get_elem("describedBy") |>
    unlist(use.names = FALSE) |>
    collapse::funique() |>
    purrr::map(request) |>
    httr2::req_perform_parallel(on_error = "continue") |>
    httr2::resps_successes()

  x |>
    purrr::map(
      function(resp)
        parse_string(resp, query = "/data") |>
        collapse::get_elem("fields") |>
        map_na_if() |>
        cheapr::as_df() |>
        collapse::mtt(
          description = rm_nonascii(description),
          description = gremove(description, "[\n\"']"),
          description = greplace(description, "[\\\\]", "-"),
          description = greplace(description, "\\s+", " ")
        )
    ) |>
    rlang::set_names(
      purrr::map(x,
        function(resp)
          parse_string(resp, query = "/data") |>
          collapse::get_elem("title")) |>
        unlist(use.names = FALSE) |>
        collapse::funique()
    ) |>
    purrr::list_rbind(names_to = "endpoint") |>
    collapse::slt(endpoint, field = name, description, type, fmt = format, enum = constraints) |>
    collapse::mtt(
      enum = NA_character_,
      fmt = ifelse(fmt == "default", NA_character_, fmt),
      catalog = "caid") |>
    map_na_if() |>
    fastplyr::as_tbl()
}

caid <- caid_dictionary()
caid

dicts <- vctrs::vec_rbind(open, caid) |>
  collapse::colorder(catalog)

pin_update(
  dicts,
  name = "dicts",
  title = "Dictionaries",
  description = "Dictionaries")
