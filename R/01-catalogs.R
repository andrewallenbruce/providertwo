#' @autoglobal
#' @noRd
catalog_caid <- function() {

  x <- fload("https://data.medicaid.gov/api/1/metastore/schemas/dataset/items?show-reference-ids")

  x <- x |>
    mtt(
      identifier  = paste0("https://data.medicaid.gov/api/1/datastore/query/", identifier, "/0"),
      modified    = as_date(modified),
      periodicity = fmt_periodicity(accrualPeriodicity),
      contact     = fmt_contactpoint(x$contactPoint),
      title       = gsub("^ ", "", title, perl = TRUE),
      title       = remove_non_ascii(title),
      title       = gsub("  ", " ", title, perl = TRUE),
      description = stri_trans_general(description, "latin-ascii"),
      description = remove_non_ascii(description),
      description = gsub("[\"']", "", description, perl = TRUE),
      description = gsub("\r\n", " ", description, perl = TRUE),
      description = gsub("  ", " ", description, perl = TRUE),
      description = cheapr_if_else(description == "Dataset.", NA_character_, description)
      ) |>
    slt(
      title,
      identifier,
      description,
      periodicity,
      modified,
      contact,
      distribution
    ) |>
    as_tbl()

  get_dist <- \(x) get_elem(x$distribution, "data", DF.as.list = TRUE)

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
    slt(x, -distribution),
    rowbind(sbt(download, N == 1), sbt(download, N > 1 & ext == "csv")) |> slt(-N, -id, -ext),
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

  list(
    main = subset_detect(main, title, pat, n = TRUE, ci = TRUE) |>
      subset_detect(title, "CoreS|Scorecard|Auto", n = TRUE),
    # FIXME temp went from 178 rows to 164
    temp = subset_detect(main, title, pat, ci = TRUE) |>
      subset_detect(title, "CoreS|Scorecard|Auto", n = TRUE) |>
      mtt(
        year = as.integer(stri_extract_first_regex(title, "[12]{1}[0-9]{3}")),
        title = case(
          grepl("Child and Adult Health Care Quality Measures", title, perl = TRUE) ~ "Child and Adult Health Care Quality Measures",
          grepl("[0-9]{4} Manage", title, perl = TRUE) ~ "Managed Care Programs by State",
          grepl("NADAC \\(National Average Drug Acquisition Cost\\)", title, perl = TRUE) ~ "NADAC",
          grepl("State Drug Utilization Data", title, perl = TRUE) ~ "State Drug Utilization Data",
          grepl("Pricing Comparison", title, perl = TRUE) ~ "Pricing Comparison for Blood Disorder Treatments",
          grepl("Product Data for Newly Reported", title, perl = TRUE) ~ "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program",
          .default = title
        ),
        description = cheapr_if_else(
          title == "Child and Adult Health Care Quality Measures",
          "Performance rates on frequently reported health care quality measures in the CMS Medicaid/CHIP Child and Adult Core Sets. Dataset contains both child and adult measures.",
          description
        )
      ) |>
      roworder(title, year) |>
      f_fill(description, periodicity) |>
      slt(
        year,
        title,
        description,
        periodicity,
        modified,
        identifier,
        download,
        dictionary
      ) |>
      roworder(title, -year) |>
      f_nest_by(.by = c(title, description, periodicity)) |>
      f_ungroup(),
    download  = sbt(download, N > 1 & ext != "csv", -id),
    scorecard = subset_detect(main, title, pat, n = TRUE, ci = TRUE) |>
      subset_detect(title, "CoreS|Scorecard|Auto")
  )
}

#' @autoglobal
#' @noRd
catalog_health <- function() {

  x <- fload("https://data.healthcare.gov/api/1/metastore/schemas/dataset/items?show-reference-ids")

  x <- x |>
    as_tbl() |>
    mtt(
      identifier  = paste0("https://data.healthcare.gov/api/1/datastore/query/", identifier, "/0"),
      title       = remove_non_ascii(title),
      description = remove_non_ascii(description),
      description = gsub("[\"']", "", description, perl = TRUE),
      description = gsub("<a href=|>|target=_blank rel=noopener noreferrer|</a|<br|@\\s", "", description, perl = TRUE),
      description = gsub("Dataset.", "Dataset", description, perl = TRUE),
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
      distribution
    )

  get_dist <- \(x) get_elem(x$distribution, "data", DF.as.list = TRUE)

  download <- new_tbl(
    title    = vec_rep_each(x$title, fnobs(get_dist(x))),
    download = get_elem(get_dist(x), "downloadURL") |> delist(),
    ext      = file_ext(download)) |>
    fcount(title, add = TRUE)

  download <- rowbind(
    sbt(download, N == 1, -N, -ext),
    sbt(download, N > 1 & ext == "csv", -N, -ext)
  )

  x <- list(slt(x, -distribution), download) |> reduce(join_on_title)

  temporal <- subset_detect(x, title, "[2][0-9]{3}") |>
    mtt(
      year = as.integer(stri_extract_first_regex(title, "[2][0-9]{3}")),
      title = stri_replace_all_regex(title, "^[2][0-9]{3}\\s", ""),
      title = stri_replace_all_regex(title, "\\s\\s?[-]?\\s?[2][0-9]{3}$", ""),
      title = stri_replace_all_regex(title, "\\s\\s?[-]?\\s?PY[2][0-9]{3}$", ""),
      title = stri_replace_all_regex(title, "[RP]Y\\s?[2][0-9]{3}\\s", ""),
      title = stri_replace_all_regex(title, "\\s[0-9]{8}$", ""),
      title = stri_replace_all_regex(title, "\\sSocrata|\\sZip\\sFile$", ""),
      title = case(
        grepl("^Benefits\\sCost", title, perl = TRUE) ~ "Benefits and Cost Sharing PUF",
        grepl("^Plan\\sCrosswalk\\sPUF", title, perl = TRUE) ~ "Plan ID Crosswalk PUF",
        .default = title
      ),
      title = gsub("  ", " ", title, perl = TRUE),
      title = str_look_remove(title, "County", "ahead")
    ) |>
    subset_detect(title, "\\.zip$|Excel$", n = TRUE) |>
    colorder(year) |>
    roworder(title, -year)

  list(
    main = subset_detect(x, title, "[2][0-9]{3}", n = TRUE),
    temp = temporal
  )
}
