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
      title       = remove_non_ascii(title),
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
    # title = cheapr_rep_each(x$title, fnobs(get_dist(x))),
    title = vec_rep_each(x$title, fnobs(get_dist(x))),
    dictionary = get_dist(x) |> get_elem("describedBy$", regex = TRUE) |> delist())

  download <- new_tbl(
    # title = cheapr_rep_each(x$title, fnobs(get_dist(x))),
    title = vec_rep_each(x$title, fnobs(get_dist(x))),
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
    main = subset_detect(main, title, pat, n = TRUE, ci = TRUE) |>
      sbt(
        stri_detect_regex(title, "CoreS|Scorecard|Auto", negate = TRUE)
      ),
    temp = subset_detect(main, title, pat, ci = TRUE) |>
      sbt(
        stri_detect_regex(title, "CoreS|Scorecard|Auto", negate = TRUE)
      ) |>
      mtt(
        year = stri_extract_first_regex(title, "[12]{1}[0-9]{3}") |> as.integer(),
        title = case(
          grepl("Child and Adult Health Care Quality Measures", title, perl = TRUE) ~ "Child and Adult Health Care Quality Measures",
          grepl("[0-9]{4} Manage", title, perl = TRUE) ~ "Managed Care Programs by State",
          grepl(
            "NADAC \\(National Average Drug Acquisition Cost\\)",
            title,
            perl = TRUE
          ) ~ "NADAC",
          grepl("State Drug Utilization Data", title, perl = TRUE) ~ "State Drug Utilization Data",
          grepl("Pricing Comparison", title, perl = TRUE) ~ "Pricing Comparison for Blood Disorder Treatments",
          grepl("Product Data for Newly Reported", title, perl = TRUE) ~ "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program",
          .default = title
        )
      ) |>
      roworder(title, -year) |>
      f_fill(periodicity) |>
      slt(
        year,
        title,
        description,
        periodicity,
        issued,
        modified,
        identifier,
        download,
        dictionary
      ),
    download = download |> sbt(N > 1) |> funique(cols = c("title", "download")),
    scorecard = subset_detect(main, title, pat, n = TRUE, ci = TRUE) |> sbt(stri_detect_regex(title, "CoreS|Scorecard|Auto"))
  )
}

#' @autoglobal
#' @noRd
catalog_health <- function() {

  x <- fload("https://data.healthcare.gov/api/1/metastore/schemas/dataset/items?show-reference-ids")

  x <- x |>
    as_tbl() |>
    mtt(
      title       = remove_non_ascii(title),
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

  get_dist <- \(x) get_elem(x$distribution, "data", DF.as.list = TRUE)

  keys <- new_df(
    title   = x$title,
    keyword = make_join_col(x, keyword)) |>
    sbt(keyword != "healthcare") |>
    mtt(keyword = stri_replace_all_regex(keyword, ", healthcare|healthcare, ", ""))

  download <- new_tbl(
    title    = vec_rep_each(x$title, fnobs(get_dist(x))),
    download = get_elem(get_dist(x), "downloadURL") |> delist()) |>
    fcount(title, add = TRUE)

  list(
    main     = list(slt(x, -keyword, -distribution), keys, sbt(download, N == 1, -N)) |> reduce(join_on_title),
    download = sbt(download, N > 1)
  )

}
