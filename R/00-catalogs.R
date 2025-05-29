#' @include utils_misc.R
#' @include utils_catalog.R
NULL

#' @autoglobal
#' @noRd
catalog_care <- function() {

  x <- fload("https://data.cms.gov/data.json", query = "/dataset")

  x <- mtt(
    x,
    api         = "Medicare",
    modified    = as_date(modified),
    periodicity = fmt_periodicity(accrualPeriodicity),
    contact     = fmt_contactpoint(x$contactPoint),
    references  = delist(references),
    temporal    = fmt_temporal(temporal),
    title       = rp_dbl_space(title),
    title       = rm_non_ascii(title),
    description = stri_trans_general(description, "latin-ascii"),
    description = rm_non_ascii(description),
    description = rm_quotes(description),
    description = gremove(description, "Note: This full dataset contains more records than most spreadsheet programs can handle, which will result in an incomplete load of data. Use of a database or statistical software is required.$"),
    description = gremove(description, "^ATTENTION USERSSome Providers Opt-Out Status may end early due to COVID 19 waivers. Please contact your respective MAC for further information. For more information on the opt-out process, see Manage Your Enrollment or view the FAQ section below. "),
    description = gremove(description, "On November 17, 2023, CMS published in the Federal Register a final rule titled, .+Medicare and Medicaid Programs; Disclosures of Ownership and Additional Disclosable Parties Information for Skilled Nursing Facilities and Nursing Facilities; Medicare Providers.+ and Suppliers.+ Disclosure of Private Equity Companies and Real Estate Investment Trusts.+ .+88 FR 80141.+. This final rule implements parts of section 1124.+c.+ \\n\\n.+\\n\\n.+"),
    description = gremove(description, "\\n\\n.+\\n\\n"),
    description = stri_trim(description)) |>
    slt(api, title, description, modified, periodicity, temporal, contact, identifier, dictionary = describedBy, site = landingPage, references, distribution) |>
    as_tbl()

  d <- rowbind(x$distribution, fill = TRUE) |>
    fcompute(
      api        = "Medicare [Temporal]",
      year       = extract_year(title),
      title      = gremove(title, " : [0-9]{4}-[0-9]{2}-[0-9]{2}([0-9A-Za-z]{1,3})?$"),
      format     = ifelse_(!is_na(description), description, format),
      modified   = as_date(modified),
      temporal   = fmt_temporal(temporal),
      identifier = accessURL,
      download   = lag_(downloadURL, n = -1L),
      resources  = resourcesAPI
    ) |>
    colorder(title) |>
    as_tbl()

  d <- sset(d, row_na_counts(d) < 4) |> funique(cols = c("title", "year", "format"))

  list_tidy(
    main = join_on_title(slt(x, -distribution),
                         sbt(d, format == "latest", title, download, resources)) |>
      roworder(title),
    temp = join_on_title(sbt(d, format != "latest" & title %!in_% care_types("single"), -format) |>
                           roworder(title, -year) |>
                           f_nest_by(.by = c(api, title)) |>
                           f_ungroup() |>
                           rnm(endpoints = data),
      slt(main, title, description, periodicity, contact, dictionary, site, references)) |>
      colorder(endpoints, pos = "end")
  )
}

#' @autoglobal
#' @noRd
catalog_pro <- function() {

  x <- fload("https://data.cms.gov/provider-data/api/1/metastore/schemas/dataset/items")

  x <- mtt(
    x,
    api         = "Provider",
    title       = rm_non_ascii(title),
    dictionary  = paste0("https://data.cms.gov/provider-data/dataset/", identifier, "#data-dictionary"),
    identifier  = paste0("https://data.cms.gov/provider-data/api/1/datastore/query/", identifier, "/0"),
    issued      = as_date(issued),
    modified    = as_date(modified),
    released    = as_date(released),
    group       = flatten_column(theme),
    description = stri_trim(gremove(description, "\n")),
    download    = delist_elem(x$distribution, "downloadURL"),
    contact     = fmt_contactpoint(x$contactPoint)) |>
    slt(api, title, group, description, issued, modified, released, identifier, contact, download, site = landingPage, dictionary) |>
    roworder(group, title) |>
    as_tbl()

  list(
   main = sbt(x, group != "Physician office visit costs")
   # cost = sbt(x, group == "Physician office visit costs")
  )
}

#' @autoglobal
#' @noRd
catalog_open <- function() {

  x <- fload("https://openpaymentsdata.cms.gov/api/1/metastore/schemas/dataset/items?show-reference-ids")

  x <- mtt(
    x,
    identifier  = paste0("https://openpaymentsdata.cms.gov/api/1/datastore/query/", identifier, "/0"),
    modified    = as_date(modified),
    year        = get_data_elem(keyword),
    year        = greplace(year, "all years", "All"),
    year        = ifelse_(title == "Provider profile ID mapping table", "All", year),
    title       = rm_non_ascii(title),
    title       = toTitleCase(title),
    contact     = fmt_contactpoint(x$contactPoint),
    description = rm_quotes(description),
    description = greplace(description, "\r\n", " "),
    description = gremove(description, "<p><strong>NOTE: </strong>This is a very large file and, depending on your network characteristics and software, may take a long time to download or fail to download. Additionally, the number of rows in the file may be larger than the maximum rows your version of <a href=https://support.microsoft.com/en-us/office/excel-specifications-and-limits-1672b34d-7043-467e-8e27-269d656771c3>Microsoft Excel</a> supports. If you cant download the file, we recommend engaging your IT support staff. If you are able to download the file but are unable to open it in MS Excel or get a message that the data has been truncated, we recommend trying alternative programs such as MS Access, Universal Viewer, Editpad or any other software your organization has available for large datasets.</p>$"),
    description = stri_trim(rp_dbl_space(description)),
    download    = get_distribution(x) |> get_elem("downloadURL") |> delist()) |>
    slt(year, title, description, modified, identifier, contact, download) |>
    as_tbl()

  list(
    main = sbt(x, year == "All", -year) |>
      mtt(api = "Open Payments") |>
      colorder(api) |>
      roworder(title),
    temp = sbt(x, year != "All") |>
      mtt(api = "Open Payments [Temporal]",
          year = as.integer(year),
          title = gremove(title, "^[0-9]{4} "),
          description = val_match(
            title,
            "General Payment Data"   ~ "All general (non-research, non-ownership related) payments from the program year",
            "Ownership Payment Data" ~ "All ownership and investment payments from the program year",
            "Research Payment Data"  ~ "All research-related payments from the program year",
            .default = description)
        ) |>
      colorder(api) |>
      roworder(title, -year) |>
      f_nest_by(.by = c(api, title, description, modified)) |>
      f_ungroup() |>
      rnm(endpoints = data)
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
      title       = gremove(title, "^ "),
      title       = rm_non_ascii(title),
      title       = rp_dbl_space(title),
      description = stri_trans_general(description, "latin-ascii"),
      description = rm_non_ascii(description),
      description = rm_quotes(description),
      description = greplace(description, "\r\n", " "),
      description = rp_dbl_space(description),
      description = ifelse_(description == "Dataset.", NA_character_, description)) |>
    slt(title, identifier, description, periodicity, modified, contact, distribution) |>
    as_tbl()

  download <- new_tbl(
    title    = cheapr_rep_each(x$title, fnobs(get_distribution(x))),
    download = get_distribution(x) |> get_elem("^downloadURL$", regex = TRUE) |> delist(),
    ext      = path_ext(download)) |>
    fcount(title, add = TRUE)

  main <- list(
    slt(x, -distribution),
    rowbind(sbt(download, N == 1, -N, -ext),
            sbt(download, N > 1 & ext == "csv", -N, -ext)),
    sbt(download, N > 2 & ext != "csv", -N, -ext) |> f_nest_by(.by = title) |> f_ungroup() |> rnm(resources = data)) |>
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
    main = subset_detect(main, title, ptn, n = TRUE, ci = TRUE) |>
      subset_detect(title, "CoreS|Scorecard|Auto", n = TRUE) |>
      mtt(api = "Medicaid") |>
      colorder(api),
    temp = subset_detect(main, title, ptn, ci = TRUE) |>
      subset_detect(title, "CoreS|Scorecard|Auto", n = TRUE) |>
      mtt(api   = "Medicaid [Temporal]",
          year  = extract_year(title),
          title = case(
            gdetect(title, "Child and Adult Health Care Quality Measures")       ~ "Child and Adult Health Care Quality Measures",
            gdetect(title, "[0-9]{4} Manage")                                    ~ "Managed Care Programs by State",
            gdetect(title, "NADAC \\(National Average Drug Acquisition Cost\\)") ~ "NADAC",
            gdetect(title, "State Drug Utilization Data")                        ~ "State Drug Utilization Data",
            gdetect(title, "Pricing Comparison")                                 ~ "Pricing Comparison for Blood Disorder Treatments",
            gdetect(title, "Product Data for Newly Reported")                    ~ "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program",
            .default = title),
          description = ifelse_(
            title == "Child and Adult Health Care Quality Measures",
            "Performance rates on frequently reported health care quality measures in the CMS Medicaid/CHIP Child and Adult Core Sets. Dataset contains both child and adult measures.",
            description)
          ) |>
      roworder(title, year) |>
      f_fill(description, periodicity) |>
      slt(api, year, title, description, periodicity, modified, identifier, download) |>
      roworder(title, -year) |>
      f_nest_by(.by = c(api, title, description, periodicity)) |>
      f_ungroup() |>
      rnm(endpoints = data)
    )
    # scorecard = subset_detect(main, title, ptn, n = TRUE, ci = TRUE) |>
    # subset_detect(title, "CoreS|Scorecard|Auto")
}

#' @autoglobal
#' @noRd
catalog_hgov <- function() {

  x <- fload("https://data.healthcare.gov/api/1/metastore/schemas/dataset/items?show-reference-ids")

  x <- x |>
    as_tbl() |>
    mtt(
      identifier  = paste0("https://data.healthcare.gov/api/1/datastore/query/", identifier, "/0"),
      title       = rm_non_ascii(title),
      title       = rp_dbl_space(title),
      description = rm_non_ascii(description),
      description = rm_quotes(description),
      description = gremove(description, "<a href=|>|target=_blank rel=noopener noreferrer|</a|<br|@\\s"),
      description = greplace(description, "Dataset.", NA_character_),
      modified    = as_date(modified),
      issued      = as_date(issued),
      periodicity = fmt_periodicity(accrualPeriodicity),
      contact     = fmt_contactpoint(x$contactPoint)) |>
    slt(title, identifier, description, periodicity, issued, modified, contact, distribution)

  download <- new_tbl(
    title    = cheapr_rep_each(x$title, fnobs(get_distribution(x))),
    download = get_distribution(x) |> get_elem("downloadURL") |> delist(),
    ext      = path_ext(download)) |>
    fcount(title, add = TRUE)

  x <- list(slt(x, -distribution),
            rowbind(sbt(download, N == 1, -N, -ext),
                    sbt(download, N > 1 & ext == "csv", -N, -ext))) |>
    reduce(join_on_title)

  qhp <- subset_detect(x, title, "QHP|SHOP")

  temporal <- subset_detect(x, title, "[2][0-9]{3}|\\sPUF") |>
    sbt(title %!in_% get_elem(qhp, "title")) |>
    subset_detect(title, "Qualifying|QHP Landscape Health Plan Business Rule Variables", n = TRUE) |>
    mtt(
      periodicity = "Annually [R/P1Y]",
      year  = extract_year(title),
      title = gremove(title, "^[2][0-9]{3}\\s"),
      title = gremove(title, "\\s\\s?[-]?\\s?[2][0-9]{3}$"),
      title = gremove(title, "\\s\\s?[-]?\\s?PY[2][0-9]{3}$"),
      title = gremove(title, "[RP]Y\\s?[2][0-9]{3}\\s"),
      title = gremove(title, "\\s[0-9]{8}$"),
      title = gremove(title, "\\sSocrata|\\sZip\\sFile$"),
      title = case(
        gdetect(title, "^Benefits\\sCost")                    ~ "Benefits and Cost Sharing PUF",
        gdetect(title, "^Plan\\sCrosswalk\\sPUF")             ~ "Plan ID Crosswalk PUF",
        gdetect(title, "Transparency [Ii]n Coverage PUF")     ~ "Transparency in Coverage PUF",
        .default = title
      ),
      title = rp_dbl_space(title),
      title = greplace(title, "/\\s", "/"),
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
        .default = description
      )
    ) |>
    subset_detect(title, "\\.zip$|Excel$", n = TRUE) |>
    colorder(year) |>
    roworder(title, -year)


  qhp <- qhp |>
    subset_detect(title, "Excel|[Zz]ip|Instructions|\\.zip$", n = TRUE) |>
    mtt(
    description = cheapr_if_else(is_na(description), title, description),
    year = extract_year(title),
    year = ifelse(is_na(year), paste0("20", stri_extract_first_regex(title, "[0-9]{2}")) |> as.integer(), year) |> suppressWarnings(),
    year = ifelse(is_na(year), substr(modified, 1, 4), year),
    title = gremove(title, "^[2][0-9]{3}\\s"),
    title = gremove(title, "\\s\\s?[-]?\\s?[2][0-9]{3}$"),
    title = gremove(title, "\\s\\s?[-]?\\s?PY[2][0-9]{3}$"),
    title = gremove(title, "[RP]Y\\s?[2][0-9]{3}\\s"),
    title = gremove(title, "[RP]Y\\s?[1][0-9]\\s"),
    title = gremove(title, "\\s[0-9]{8}$"),
    title = rp_dbl_space(title),
    title = greplace(title, "/\\s", "/"),
    title = case(
      gdetect(title, "QHP\\sDent[-]\\sIndi[-]\\s")          ~ "QHP Landscape Individual Market Dental",
      gdetect(title, "QHP Landscape Individual Dental")     ~ "QHP Landscape Individual Market Dental",
      gdetect(title, "QHP\\sMedi[-]\\sIndi[-]\\s")          ~ "QHP Landscape Individual Market Medical",
      gdetect(title, "QHP Landscape Individual Medical")    ~ "QHP Landscape Individual Market Medical",
      gdetect(title, "QHP\\sMedi[-]\\sSHOP[-]\\s")          ~ "QHP Landscape SHOP Market Medical",
      gdetect(title, "QHP\\sMedical\\s[-]\\sSHOP\\s[-]\\s") ~ "QHP Landscape SHOP Market Medical",
      gdetect(title, "QHP Landscape Medical SHOP")          ~ "QHP Landscape SHOP Market Medical",
      gdetect(title, "QHP\\sDent[-]\\sSHOP[-]\\s")          ~ "QHP Landscape SHOP Market Dental",
      gdetect(title, "QHP Landscape Dental SHOP")           ~ "QHP Landscape SHOP Market Dental",
      .default = title)
    ) |>
    colorder(year) |>
    roworder(title, -year)

  list(
    main = rowbind(subset_detect(x, title, "[2][0-9]{3}|QHP|SHOP|\\sPUF", n = TRUE),
                   subset_detect(x, title, "Qualifying|QHP Landscape Health Plan Business Rule Variables")) |>
      mtt(api = "HealthcareGov",
          title = greplace(title, "/\\s", "/"),
          title = greplace(title, "Qualifying Health Plan", "QHP"),
          title = str_look_remove(title, "County", "ahead"),
          title = gremove(title, "2015 ")) |>
      colorder(api),
    temp = rowbind(temporal, subset_detect(qhp, title, "^QHP Landscape [HINO][IDMVR]|^QHP Landscape Health Plan Business Rule Variables", n = TRUE)) |>
      mtt(api = "HealthcareGov [Temporal]") |>
      colorder(api) |>
      f_nest_by(.by = c(api, title)) |>
      f_ungroup() |>
      rnm(endpoints = data)
    # qhp = subset_detect(qhp, title, "^QHP Landscape [HINO][IDMVR]|^QHP Landscape Health Plan Business Rule Variables", n = TRUE) |>
    #   mtt(api = "HealthcareGov [Temporal]") |>
    #   colorder(api) |>
    #   f_nest_by(.by = c(api, title)) |>
    #   f_ungroup() |>
    #   rnm(endpoints = data),
    # states = subset_detect(qhp, title, "^QHP Landscape [HINO][IDMVR]")
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

the          <- new.env(parent = emptyenv())
the$catalogs <- catalogs()

#' @autoglobal
#' @noRd
reset_catalog <- function() {
  old <- the$catalogs
  the$catalogs <- catalogs()
  invisible(old)
}
