#' @autoglobal
#' @noRd
make_join_col <- \(x, col) {
  map(x[[ensym(col)]], function(x) get_elem(as.list(x), "data")) |>
    flatten_column() |>
    na_if("")
}

#' @autoglobal
#' @noRd
dict_title <- \(x) {
  x |>
    request() |>
    perform_simple() |>
    _[["data"]] |>
    _[["title"]]
}

#' @autoglobal
#' @noRd
perform_parallel <- function(x) {
  resp_list <- map(x, request) |>
    req_perform_parallel(on_error = "continue")

  parse_string <- \(x) fparse(resp_body_string(x), query = "/data")

  resps_successes(resp_list) |>
    map(parse_string) |>
    rowbind() |>
    map_na_if()
}

#' @autoglobal
#' @noRd
catalog_caid <- function() {

  x <- fload("https://data.medicaid.gov/api/1/metastore/schemas/dataset/items?show-reference-ids")

  x <- x |>
    as_tbl() |>
    mtt(
      modified    = as_date(modified),
      issued      = as_date(issued),
      periodicity = fmt_periodicity(accrualPeriodicity),
      contact     = fmt_contactpoint(x$contactPoint)
    ) |>
    slt(
      title,
      identifier,
      description,
      periodicity,
      issued,
      modified,
      contact,
      theme,
      keyword,
      distribution,
      temporal,
      references
    )

  groups <- new_df(title = x$title,
                   group = make_join_col(x, theme)) |>
    sbt(not_na(group) & group != "Uncategorized")

  keys <- new_df(title = x$title, keyword = make_join_col(x, keyword))

  refs <- new_df(title = x$title,
                 references = na_if(flatten_column(x$references), "NA")) |>
    sbt(not_na(references) & stringi::stri_detect_regex(references, "^https://www.mathematica.org/", negate = TRUE)) |>
    mtt(references = stringi::stri_replace_all_fixed(references, ", https://www.mathematica.org/", ""))

  d <- rowbind(x$distribution, fill = TRUE)

  tvec <- vctrs::vec_rep_each(x$title, fnobs(get_elem(x$distribution, "data", DF.as.list = TRUE)))

  download <- new_tbl(title = tvec, download = delist(get_elem(d$data, "downloadURL"))) |> fcount(title, add = TRUE)

  list(
    sets       = reduce(list(slt(x, -theme, -keyword, -references, -distribution), groups, keys, refs, sbt(download, N == 1, -N)), join_on_title),
    download   = funique(sbt(download, N > 1), cols = "download"),
    dictionary = new_tbl(url = funique(delist(get_elem(d$data, "describedBy")))) # |>
      # mtt(identifier = stringi::stri_extract(url, regex = "(?:[0-9a-fA-F]){8}-?(?:[0-9a-fA-F]){4}-?(?:[0-9a-fA-F]){4}-?(?:[0-9a-fA-F]){4}-?(?:[0-9a-fA-F]){12}")) |>
      # mtt(title = map_chr(url, \(x) dict_title(x))) |>
      # colorder(title, identifier)
  )
}
