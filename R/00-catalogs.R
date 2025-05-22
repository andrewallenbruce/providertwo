#' @include utils_misc.R
#' @include utils_catalog.R
NULL

#' @autoglobal
#' @noRd
catalog_care <- function() {

  x <- fload("https://data.cms.gov/data.json", query = "/dataset")

  x <- mtt(
    x,
    modified    = as_date(modified),
    periodicity = fmt_periodicity(accrualPeriodicity),
    contact     = fmt_contactpoint(x$contactPoint),
    references  = delist(references),
    temporal    = fmt_temporal(temporal),
    title       = greplace(title, "  ", " "),
    title       = remove_non_ascii(title),
    description = stri_trans_general(description, "latin-ascii"),
    description = remove_non_ascii(description),
    description = greplace(description, "[\"']", ""),
    description = greplace(description, "Note: This full dataset contains more records than most spreadsheet programs can handle, which will result in an incomplete load of data. Use of a database or statistical software is required.$", ""),
    description = greplace(description, "^ATTENTION USERSSome Providers Opt-Out Status may end early due to COVID 19 waivers. Please contact your respective MAC for further information. For more information on the opt-out process, see Manage Your Enrollment or view the FAQ section below. ", ""),
    description = greplace(description, "On November 17, 2023, CMS published in the Federal Register a final rule titled, .+Medicare and Medicaid Programs; Disclosures of Ownership and Additional Disclosable Parties Information for Skilled Nursing Facilities and Nursing Facilities; Medicare Providers.+ and Suppliers.+ Disclosure of Private Equity Companies and Real Estate Investment Trusts.+ .+88 FR 80141.+. This final rule implements parts of section 1124.+c.+ \\n\\n.+\\n\\n.+", ""),
    description = greplace(description, "\\n\\n.+\\n\\n", ""),
    description = stri_trim(description)
  ) |>
    slt(
      title,
      description,
      modified,
      periodicity,
      temporal,
      contact,
      identifier,
      dictionary  = describedBy,
      site        = landingPage,
      references,
      distribution
    ) |>
    as_tbl()

  d <- rowbind(x$distribution, fill = TRUE) |>
    fcompute(
      year       = extract_year(title),
      title      = stri_replace_all_regex(title, " : [0-9]{4}-[0-9]{2}-[0-9]{2}([0-9A-Za-z]{1,3})?$", ""),
      format     = cheapr_if_else(!is_na(description), description, format),
      modified   = as_date(modified),
      temporal   = fmt_temporal(temporal),
      identifier = accessURL,
      download   = lag_(downloadURL, n = -1L),
      filetype   = lag_(mediaType, n = -1L),
      resources  = resourcesAPI
    ) |>
    colorder(title) |>
    as_tbl()

  d <- sset(d, row_na_counts(d) < 4) |> funique(cols = c("title", "year", "format"))

  list_tidy(
    main = join_on_title(
      slt(x, -distribution),
      sbt(d, format == "latest", title, download, resources)) |> roworder(title),
    temp = join_on_title(
      sbt(d, format != "latest" & title %!in_% care_types("single"), -format, -filetype) |>
        roworder(title, -year) |>
        f_nest_by(.by = title) |>
        f_ungroup(),
      slt(main, title, description, periodicity, contact, dictionary, site)
    )
  )
}

# "https://data.cms.gov/provider-data/sites/default/files/data_dictionaries/physician/DOC_Data_Dictionary.pdf"
#' @autoglobal
#' @noRd
catalog_pro <- function() {
  x <- fload("https://data.cms.gov/provider-data/api/1/metastore/schemas/dataset/items")

  x <- mtt(x,
    title       = remove_non_ascii(title),
    dictionary  = paste0("https://data.cms.gov/provider-data/dataset/", identifier, "#data-dictionary"),
    identifier  = paste0("https://data.cms.gov/provider-data/api/1/datastore/query/", identifier, "/0"),
    issued      = as_date(issued),
    modified    = as_date(modified),
    released    = as_date(released),
    group       = flatten_column(theme),
    description = stri_trim(greplace(description, "\n", "")),
    download    = delist_elem(x$distribution, "downloadURL"),
    contact     = fmt_contactpoint(x$contactPoint)) |>
    slt(title, group, description, issued, modified, released, identifier, contact, download, site = landingPage, dictionary) |>
    roworder(group, title) |>
    as_tbl()

  list(
   end  = sbt(x, group != "Physician office visit costs"),
   cost = sbt(x, group == "Physician office visit costs")
  )
}

#' @autoglobal
#' @noRd
catalog_open <- function() {

  x <- fload("https://openpaymentsdata.cms.gov/api/1/metastore/schemas/dataset/items?show-reference-ids")

  x <- mtt(x,
    identifier  = paste0("https://openpaymentsdata.cms.gov/api/1/datastore/query/", identifier, "/0"),
    modified    = as_date(modified),
    year        = get_data_elem(keyword),
    year        = greplace(year, "all years", "All"),
    year        = cheapr_if_else(title == "Provider profile ID mapping table", "All", year),
    title       = remove_non_ascii(title),
    title       = toTitleCase(title),
    contact     = fmt_contactpoint(x$contactPoint),
    description = greplace(description, "[\"']", ""),
    description = greplace(description, "\r\n", " "),
    description = greplace(description, "<p><strong>NOTE: </strong>This is a very large file and, depending on your network characteristics and software, may take a long time to download or fail to download. Additionally, the number of rows in the file may be larger than the maximum rows your version of <a href=https://support.microsoft.com/en-us/office/excel-specifications-and-limits-1672b34d-7043-467e-8e27-269d656771c3>Microsoft Excel</a> supports. If you cant download the file, we recommend engaging your IT support staff. If you are able to download the file but are unable to open it in MS Excel or get a message that the data has been truncated, we recommend trying alternative programs such as MS Access, Universal Viewer, Editpad or any other software your organization has available for large datasets.</p>$", ""),
    description = greplace(description, "  ", " "),
    description = stri_trim(description),
    download    = get_elem(x$distribution, "data", DF.as.list = TRUE) |> get_elem("downloadURL") |> delist()) |>
    slt(year,
        title,
        description,
        modified,
        identifier,
        contact,
        download) |>
    as_tbl()

  list(
    main = sbt(x, year == "All", -year) |> roworder(title),
    temp = sbt(x, year != "All") |>
      mtt(year        = as.integer(year),
          title       = stri_replace_all_regex(title, "^[0-9]{4} ", ""),
          description = case(title == "General Payment Data"   ~ "All general (non-research, non-ownership related) payments from the program year",
                             title == "Ownership Payment Data" ~ "All ownership and investment payments from the program year",
                             title == "Research Payment Data"  ~ "All research-related payments from the program year",
                             .default = description)) |>
      roworder(title, -year) |>
      f_nest_by(.by = c(title, description, modified)) |>
      f_ungroup()
    )
}

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
      title       = greplace(title, "^ ", ""),
      title       = remove_non_ascii(title),
      title       = greplace(title, "\\s\\s", "\\s"),
      description = stri_trans_general(description, "latin-ascii"),
      description = remove_non_ascii(description),
      description = greplace(description, "[\"']", ""),
      description = greplace(description, "\r\n", " "),
      description = greplace(description, "  ", " "),
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

  # dictionary <- new_df(
  #   title      = cheapr_rep_each(x$title, fnobs(get_distribution(x))),
  #   dictionary = get_distribution(x) |> get_elem("describedBy$", regex = TRUE) |> delist())

  download <- new_tbl(
    title    = cheapr_rep_each(x$title, fnobs(get_distribution(x))),
    download = get_distribution(x) |> get_elem("^downloadURL$", regex = TRUE) |> delist(),
    ext      = path_ext(download),
    id       = groupid(title)
  ) |>
    fcount(id, add = TRUE)

  main <- list(
    slt(x, -distribution),
    rowbind(sbt(download, N == 1), sbt(download, N > 1 & ext == "csv")) |> slt(-N, -id, -ext),
    sbt(download, N > 2 & ext != "csv", -id, -N, -ext) |> f_nest_by(.by = title) |> f_ungroup() |> rnm(resources = data)) |>
    reduce(join_on_title) |>
    roworder(title)

  ptn <- paste0(
    "State Drug Utilization Data [0-9]{4}|",
    "NADAC \\(National Average Drug Acquisition Cost\\)|",
    "^[0-9]{4} Child and Adult Health Care Quality|",
    "^[0-9]{4} Managed Care Programs by State$|",
    "^Pricing Comparison for Blood Disorder Treatments|",
    "^Child and Adult Health Care Quality Measures|",
    "^Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program [0-9]{2}"
  )

  list(
    main = subset_detect(main, title, ptn, n = TRUE, ci = TRUE) |> subset_detect(title, "CoreS|Scorecard|Auto", n = TRUE),
    temp = subset_detect(main, title, ptn, ci = TRUE) |> subset_detect(title, "CoreS|Scorecard|Auto", n = TRUE) |>
      mtt(year = extract_year(title),
          title = case(gdetect(title, "Child and Adult Health Care Quality Measures")       ~ "Child and Adult Health Care Quality Measures",
                       gdetect(title, "[0-9]{4} Manage")                                    ~ "Managed Care Programs by State",
                       gdetect(title, "NADAC \\(National Average Drug Acquisition Cost\\)") ~ "NADAC",
                       gdetect(title, "State Drug Utilization Data")                        ~ "State Drug Utilization Data",
                       gdetect(title, "Pricing Comparison")                                 ~ "Pricing Comparison for Blood Disorder Treatments",
                       gdetect(title, "Product Data for Newly Reported")                    ~ "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program",
                       .default = title),
          description = cheapr_if_else(title == "Child and Adult Health Care Quality Measures", "Performance rates on frequently reported health care quality measures in the CMS Medicaid/CHIP Child and Adult Core Sets. Dataset contains both child and adult measures.", description)) |>
      roworder(title, year) |>
      f_fill(description, periodicity) |>
      slt(year, title, description, periodicity, modified, identifier, download) |>
      roworder(title, -year) |>
      f_nest_by(.by = c(title, description, periodicity)) |>
      f_ungroup()
    # scorecard = subset_detect(main, title, ptn, n = TRUE, ci = TRUE) |> subset_detect(title, "CoreS|Scorecard|Auto")
  )
}

#' @autoglobal
#' @noRd
catalog_hgov <- function() {

  x <- fload("https://data.healthcare.gov/api/1/metastore/schemas/dataset/items?show-reference-ids")

  x <- x |>
    as_tbl() |>
    mtt(
      identifier  = paste0("https://data.healthcare.gov/api/1/datastore/query/", identifier, "/0"),
      title       = remove_non_ascii(title),
      title       = greplace(title, "  ", " "),
      description = remove_non_ascii(description),
      description = greplace(description, "[\"']", ""),
      description = greplace(description, "<a href=|>|target=_blank rel=noopener noreferrer|</a|<br|@\\s", ""),
      description = greplace(description, "Dataset.", NA_character_),
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

  download <- new_tbl(
    title    = cheapr_rep_each(x$title, fnobs(get_distribution(x))),
    download = get_elem(get_distribution(x), "downloadURL") |> delist(),
    ext      = path_ext(download)) |>
    fcount(title, add = TRUE)

  download <- rowbind(
    sbt(download, N == 1, -N, -ext),
    sbt(download, N > 1 & ext == "csv", -N, -ext)
  )

  x <- list(slt(x, -distribution), download) |> reduce(join_on_title)

  temporal <- subset_detect(x, title, "[2][0-9]{3}") |>
    mtt(
      year  = extract_year(title),
      title = stri_replace_all_regex(title, "^[2][0-9]{3}\\s", ""),
      title = stri_replace_all_regex(title, "\\s\\s?[-]?\\s?[2][0-9]{3}$", ""),
      title = stri_replace_all_regex(title, "\\s\\s?[-]?\\s?PY[2][0-9]{3}$", ""),
      title = stri_replace_all_regex(title, "[RP]Y\\s?[2][0-9]{3}\\s", ""),
      title = stri_replace_all_regex(title, "\\s[0-9]{8}$", ""),
      title = stri_replace_all_regex(title, "\\sSocrata|\\sZip\\sFile$", ""),
      title = case(
        gdetect(title, "^Benefits\\sCost")                    ~ "Benefits and Cost Sharing PUF",
        gdetect(title, "^Plan\\sCrosswalk\\sPUF")             ~ "Plan ID Crosswalk PUF",
        gdetect(title, "Transparency [Ii]n Coverage PUF")     ~ "Transparency in Coverage PUF",
        gdetect(title, "QHP\\sDent[-]\\sIndi[-]\\s")          ~ "QHP Landscape Individual Dental",
        gdetect(title, "QHP\\sMedi[-]\\sIndi[-]\\s")          ~ "QHP Landscape Individual Medical",
        gdetect(title, "QHP\\sMedical\\s[-]\\sSHOP\\s[-]\\s") ~ "QHP Landscape Medical SHOP",
        gdetect(title, "QHP\\sDent[-]\\sSHOP[-]\\s")          ~ "QHP Landscape Dental SHOP",
        .default = title
      ),
      title = greplace(title, "  ", " "),
      title = greplace(title, "/\\s", "/"),
      title = str_look_remove(title, "County", "ahead"),
      description = case(
        gdetect(title, "Benefits and Cost Sharing PUF") ~ "The Benefits and Cost Sharing PUF (BenCS-PUF) is one of the files that comprise the Health Insurance Exchange Public Use Files. The BenCS-PUF contains plan variant-level data on essential health benefits, coverage limits, and cost sharing for each QHP and SADP.",
        gdetect(title, "Business Rules PUF")            ~ "The Business Rules PUF (BR-PUF) is one of the files that comprise the Health Insurance Exchange Public Use Files. The BR-PUF contains plan-level data on rating business rules, such as maximum age for a dependent and allowed dependent relationships.",
        gdetect(title, "MLR Dataset")                   ~ "This file contains Medical Loss Ratio data for the Reporting Year, including the issuers MLR, the MLR standard, and the average rebate per family by state and market.",
        gdetect(title, "Machine Readable PUF")          ~ "The Machine Readable PUF (MR-PUF) is one of the files that comprise the Health Insurance Exchange Public Use Files. The MR-PUF contains issuer-level URL locations for machine-readable network provider and formulary information.",
        gdetect(title, "Network PUF")                   ~ "The Network PUF (Ntwrk-PUF) is one of the files that comprise the Health Insurance Exchange Public Use Files. The Ntwrk-PUF contains issuer-level data identifying provider network URLs.",
        gdetect(title, "Plan Attributes PUF")           ~ "The Plan Attributes PUF (Plan-PUF) is one of the files that comprise the Health Insurance Exchange Public Use Files. The Plan-PUF contains plan variant-level data on maximum out of pocket payments, deductibles, health savings account (HSA) eligibility, and other plan attributes.",
        gdetect(title, "Plan ID Crosswalk PUF")         ~ "The Plan ID Crosswalk PUF (CW-PUF) is one of the files that comprise the Health Insurance Exchange Public Use Files. The purpose of the CW-PUF is to map QHPs and SADPs offered through the Exchanges during the previous plan year to plans that will be offered through the Exchanges in the current plan year.",
        gdetect(title, "Quality PUF")                   ~ "The Quality PUF contains yearly quality ratings data for eligible Qualified Health Plans in states on HealthCare.gov.",
        gdetect(title, "Rate PUF")                      ~ "The Rate PUF (Rate-PUF) is one of the files that comprise the Health Insurance Exchange Public Use Files. The Rate-PUF contains plan-level data on rates based on an eligible subscribers age, tobacco use, and geographic location; and family-tier rates.",
        gdetect(title, "Service Area PUF")              ~ "The Service Area PUF (SA-PUF) is one of the files that comprise the Health Insurance Exchange Public Use Files. The SA-PUF contains issuer-level data on geographic service areas including state, county, and zip code.",
        gdetect(title, "Transparency in Coverage PUF")  ~ "The Transparency in Coverage PUF (TC-PUF) is one of the files that comprise the Health Insurance Exchange Public Use Files. The PUF contains data on issuer and plan-level claims, appeals, and active URL data.",
        .default = description),
      periodicity = "Annually [R/P1Y]"
    ) |>
    subset_detect(title, "\\.zip$|Excel$", n = TRUE) |>
    colorder(year) |>
    roworder(title, -year) |>
    f_nest_by(.by = c(title, description, periodicity)) |>
    f_ungroup()

  list(
    main = subset_detect(x, title, "[2][0-9]{3}", n = TRUE),
    temp = subset_detect(temporal, title, "^QHP|^SHOP|^Qualifying", n = TRUE),
    qhp  = subset_detect(temporal, title, "^QHP|^SHOP|^Qualifying")
  )
}


#' @name catalogs
#' @title API Catalogs
#' @description
#' List of API catalogs:
#'   * `catalog_care`: CMS Medicare API
#'   * `catalog_pro`: CMS Provider API
#'   * `catalog_open`: CMS Open Payments API
#'   * `catalog_caid`: CMS Medicaid API
#'   * `catalog_hgov`: CMS HealthCare.gov API
#' @returns `<list>` of catalogs
#' @examples
#' catalogs()
#' @autoglobal
#' @export
catalogs <- function() {
  list(
    care = catalog_care(),
    pro  = catalog_pro(),
    open = catalog_open(),
    caid = catalog_caid(),
    hgov = catalog_hgov()
  )
}

# rlang::on_load(.catalog <<- catalogs())
# check_catalog_exists <- function() {
#   if (!exists(".catalog")) .catalog <<- catalogs()
# }

the          <- new.env(parent = emptyenv())
the$catalogs <- catalogs()

#' @autoglobal
#' @noRd
reset_catalog <- function() {
  old <- the$catalogs
  the$catalogs <- catalogs()
  invisible(old)
}
