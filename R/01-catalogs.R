#' @autoglobal
#' @noRd
str_look <- function(pattern, look) {
  switch(
    match.arg(look, c("ahead", "behind")),
    ahead  = glue("(?<={pattern}).*$"),
    behind = glue("^.*(?={pattern})")
  )
}

#' @autoglobal
#' @noRd
str_look_detect <- function(x, pattern, look) {
  str_look(pattern, look) |>
    grepl(x, perl = TRUE)
}

#' @autoglobal
#' @noRd
str_look_replace <- function(x, pattern, look, replacement) {
  str_look(pattern, look) |>
    gsub(replacement = replacement, x, perl = TRUE)
}

#' @autoglobal
#' @noRd
str_look_remove <- function(x, pattern, look) {
  str_look_replace(x, pattern, look, replacement = "")
}


#' @autoglobal
#' @noRd
make_join_col <- \(x, col) {
  map(x[[ensym(col)]], function(x) get_elem(as.list(x), "data")) |>
    flatten_column() |>
    na_if("")
}

#' @autoglobal
#' @noRd
catalog_caid <- function() {

  x <- fload("https://data.medicaid.gov/api/1/metastore/schemas/dataset/items?show-reference-ids")

  x <- x |>
    mtt(
      identifier  = paste0("https://data.medicaid.gov/api/1/datastore/query/", identifier, "/0"),
      modified    = as_date(modified),
      issued      = as_date(issued),
      periodicity = fmt_periodicity(accrualPeriodicity),
      contact     = fmt_contactpoint(x$contactPoint),
      title       = gsub("^ ", "", title, perl = TRUE),
      description = stri_trans_general(description, "latin-ascii"),
      description = remove_non_ascii(description),
      description = gsub("[\"']", "", description, perl = TRUE),
      description = gsub("\r\n", " ", description, perl = TRUE),
      description = gsub("  ", " ", description, perl = TRUE)
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
      key = keyword,
      distribution,
      temporal,
      reference = references
    ) |>
    as_tbl()

  get_dist <- \(x) get_elem(x$distribution, "data", DF.as.list = TRUE)

  grps <- new_df(title = x$title, group = make_join_col(x, theme)) |> sbt(not_na(group) & group != "Uncategorized")
  keys <- new_df(title = x$title, key = make_join_col(x, key))

  refs <- new_df(
    title = x$title,
    reference = flatten_column(x$reference) |> na_if("NA")) |>
    sbt(not_na(reference) & stri_detect_regex(reference, "^https://www.mathematica.org/", negate = TRUE)) |>
    mtt(reference = stri_replace_all_fixed(reference, ", https://www.mathematica.org/", ""))

  dictionary <- new_df(
    title = cheapr_rep_each(x$title, fnobs(get_dist(x))),
    dictionary = get_dist(x) |> get_elem("describedBy$", regex = TRUE) |> delist())

  download <- new_tbl(
    title = cheapr_rep_each(x$title, fnobs(get_dist(x))),
    download = get_dist(x) |> get_elem("^downloadURL$", regex = TRUE) |> delist(),
    ext = file_ext(download),
    id = groupid(title)) |>
    fcount(id, add = TRUE)

  main <- list(
    slt(x, -theme, -key, -reference, -distribution),
    grps,
    keys,
    refs,
    download |> sbt(N == 1, -N, -id, -ext),
    dictionary) |>
    reduce(join_on_title) |>
    roworder(title)

  pat <- paste0(
    "State Drug Utilization Data [0-9]{4}|",
    "NADAC \\(National Average Drug Acquisition Cost\\)|",
    "^[0-9]{4} Child and Adult Health Care Quality|",
    "^[0-9]{4} Managed Care Programs by State$|",
    "^Pricing Comparison for Blood Disorder Treatments|",
    "^Child and Adult Health Care Quality Measures|",
    "^Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program [0-9]{2}"
  )

  pat2 <- paste0(
    " [12][0-9]{3}|",
    "[12][0-9]{3} |",
    " \\(National Average Drug Acquisition Cost\\)|",
    " \\(Pricing as of.*|",
    " Quality$|"
  )

  list(
    main = subset_detect(main, title, pat, n = TRUE, ci = TRUE),
    temp = subset_detect(main, title, pat, ci = TRUE) |>
      mtt(
        year = stri_extract_first_regex(title, "[12]{1}[0-9]{3}") |> as.integer()
        # title = stri_replace_all_regex(title, pat2, "")
        # title = stri_replace_all_regex(title, " [0-9]{2,8}-?.*", "")
        ) |>
      colorder(year) |>
      roworder(title, -year),
    download = download |>
      sbt(N > 1) |>
      funique(cols = c("title", "download"))
  )
}

#' @autoglobal
#' @noRd
catalog_health <- function() {

  x <- fload("https://data.healthcare.gov/api/1/metastore/schemas/dataset/items?show-reference-ids")

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
      keyword,
      distribution
    )

  keys <- new_df(
    title = x$title,
    keyword = make_join_col(x, keyword)) |>
    sbt(keyword != "healthcare") |>
    mtt(keyword = stri_replace_all_regex(keyword, ", healthcare|healthcare, ", ""))

  d <- rowbind(x$distribution, fill = TRUE)

  trep <- vec_rep_each(
    x$title,
    get_elem(x$distribution, "data", DF.as.list = TRUE) |>
      fnobs())

  download <- new_tbl(
    title = trep,
    download = get_elem(d$data, "downloadURL") |>
      delist()) |>
    fcount(title, add = TRUE)

  list(
    main = list(
      slt(x, -keyword, -distribution),
      keys,
      download |>
        sbt(N == 1) |>
        slt(-N)) |>
      reduce(join_on_title),
    download = download |>
      sbt(N > 1)
  )

}
