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
    download  = sbt(download, N > 1 & ext != "csv", -id) |>
      f_nest_by(.by = title) |>
      f_ungroup(),
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
      year  = as.integer(stri_extract_first_regex(title, "[2][0-9]{3}")),
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
      title = str_look_remove(title, "County", "ahead"),
      description = case(
        title == "Benefits and Cost Sharing PUF" ~ "The Benefits and Cost Sharing PUF (BenCS-PUF) is one of the files that comprise the Health Insurance Exchange Public Use Files. The BenCS-PUF contains plan variant-level data on essential health benefits, coverage limits, and cost sharing for each QHP and SADP.",
        title == "Business Rules PUF" ~ "The Business Rules PUF (BR-PUF) is one of the files that comprise the Health Insurance Exchange Public Use Files. The BR-PUF contains plan-level data on rating business rules, such as maximum age for a dependent and allowed dependent relationships.",
        title == "MLR Dataset" ~ "This file contains Medical Loss Ratio data for the 2019 Reporting Year, including the issuers MLR, the MLR standard, and the average rebate per family by state and market.",
        title == "Machine Readable PUF" ~ "The Machine Readable PUF (MR-PUF) is one of the files that comprise the Health Insurance Exchange Public Use Files. The MR-PUF contains issuer-level URL locations for machine-readable network provider and formulary information.",
        title == "Network PUF" ~ "The Network PUF (Ntwrk-PUF) is one of the files that comprise the Health Insurance Exchange Public Use Files. The Ntwrk-PUF contains issuer-level data identifying provider network URLs.",
        title == "Plan Attributes PUF" ~ "The Plan Attributes PUF (Plan-PUF) is one of the files that comprise the Health Insurance Exchange Public Use Files. The Plan-PUF contains plan variant-level data on maximum out of pocket payments, deductibles, health savings account (HSA) eligibility, and other plan attributes.",
        title == "Plan ID Crosswalk PUF" ~ "The Plan ID Crosswalk PUF (CW-PUF) is one of the files that comprise the Health Insurance Exchange Public Use Files. The purpose of the CW-PUF is to map QHPs and SADPs offered through the Exchanges during the previous plan year to plans that will be offered through the Exchanges in the current plan year.",
        title == "Quality PUF" ~ "The Quality PUF contains 2024 quality ratings data for eligible Qualified Health Plans in PY2025 in states on HealthCare.gov.",
        title == "Rate PUF" ~ "The Rate PUF (Rate-PUF) is one of the files that comprise the Health Insurance Exchange Public Use Files. The Rate-PUF contains plan-level data on rates based on an eligible subscribers age, tobacco use, and geographic location; and family-tier rates.",
        title == "Service Area PUF" ~ "The Service Area PUF (SA-PUF) is one of the files that comprise the Health Insurance Exchange Public Use Files. The SA-PUF contains issuer-level data on geographic service areas including state, county, and zip code.",
        title == "Transparency In Coverage PUF" ~ "The Transparency in Coverage PUF (TC-PUF) is one of the files that comprise the Health Insurance Exchange Public Use Files. The PUF contains data on issuer and plan-level claims, appeals, and active URL data.",
        .default = description
      )
    ) |>
    subset_detect(title, "\\.zip$|Excel$", n = TRUE) |>
    colorder(year) |>
    roworder(title, year) |>
    f_fill(periodicity) |>
    roworder(title, -year) |>
    f_nest_by(.by = c(title, description)) |>
    f_ungroup()

  list(
    main = subset_detect(x, title, "[2][0-9]{3}", n = TRUE),
    temp = temporal
  )
}
