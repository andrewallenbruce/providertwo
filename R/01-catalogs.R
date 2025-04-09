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
catalog_caid <- function() {

  x <- fload("https://data.medicaid.gov/api/1/metastore/schemas/dataset/items?show-reference-ids")

  x <- x |>
    mtt(
      modified    = as_date(modified),
      issued      = as_date(issued),
      periodicity = fmt_periodicity(accrualPeriodicity),
      contact     = fmt_contactpoint(x$contactPoint),
      title       = gsub("^ ", "", title, perl = TRUE)) |>
    slt(title, identifier, description, periodicity, issued, modified, contact, theme, key = keyword, distribution, temporal, reference = references) |>
    as_tbl()

  grps <- new_df(title = x$title, group = make_join_col(x, theme)) |> sbt(not_na(group) & group != "Uncategorized")

  keys <- new_df(title = x$title, key = make_join_col(x, key))

  refs <- new_df(title = x$title, reference = flatten_column(x$reference) |> na_if("NA")) |>
          sbt(not_na(reference) & stri_detect_regex(reference, "^https://www.mathematica.org/", negate = TRUE)) |>
          mtt(reference = stri_replace_all_fixed(reference, ", https://www.mathematica.org/", ""))

  dict <- new_tbl(title = vec_rep_each(x$title, fnobs(get_elem(x$distribution, "data", DF.as.list = TRUE))),
                  dictionary = get_elem(get_elem(x$distribution, "data", DF.as.list = TRUE), "describedBy$", regex = TRUE) |> delist())

  dwns <- new_tbl(title = vec_rep_each(x$title, fnobs(get_elem(x$distribution, "data", DF.as.list = TRUE))),
                  download = get_elem(get_elem(x$distribution, "data", DF.as.list = TRUE), "^downloadURL$", regex = TRUE) |> delist()) |>
    fcount(title, add = TRUE)

  main <- reduce(list(slt(x, -theme, -key, -reference, -distribution), grps, keys, refs), join_on_title) |> roworder(title)

  list(
    main = sbt(main, stri_detect_regex(title, "State Drug Utilization Data [0-9]{4}|NADAC \\(National Average Drug Acquisition Cost\\)|^[0-9]{4} Child and Adult Health Care Quality|^[0-9]{4} Managed Care Programs by State$", negate = TRUE, case_insensitive = TRUE)),
    temp = sbt(main, stri_detect_regex(title, "State Drug Utilization Data [0-9]{4}|NADAC \\(National Average Drug Acquisition Cost\\)|^[0-9]{4} Child and Adult Health Care Quality|^[0-9]{4} Managed Care Programs by State$", case_insensitive = TRUE)) |> mtt(year = stri_extract_first_regex(title, "[12]{1}[0-9]{3}") |> as.integer(), title = stri_replace_all_regex(title, " [12][0-9]{3}|[12][0-9]{3} | \\(National Average Drug Acquisition Cost\\)", "")) |> colorder(year) |> roworder(title, -year),
    down = funique(dwns, cols = c("title", "download")),
    dict = funique(dict, cols = c("title", "dictionary"))
  )
}
