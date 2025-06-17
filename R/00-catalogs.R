# options(fastplyr.inform = FALSE)

#' @include utils_misc.R
#' @include fastplyr.R
NULL

#' @name catalogs
#' @title API Catalogs
#' @description
#' List of API catalogs:
#'   * `care`: CMS Medicare API
#'   * `prov`: CMS Provider API
#'   * `open`: CMS Open Payments API
#'   * `caid`: CMS Medicaid API
#'   * `hgov`: CMS HealthCare.gov API
#' @returns `<list>` of catalogs
#' @examples
#' catalogs()
#' @autoglobal
#' @export
catalogs <- function() {

  x <- c(care = "https://data.cms.gov/data.json",
         prov = "https://data.cms.gov/provider-data/api/1/metastore/schemas/dataset/items",
         open = "https://openpaymentsdata.cms.gov/api/1/metastore/schemas/dataset/items",
         caid = "https://data.medicaid.gov/api/1/metastore/schemas/dataset/items",
         hgov = "https://data.healthcare.gov/api/1/metastore/schemas/dataset/items") |>
    map(request) |>
    req_perform_parallel(on_error = "continue") |>
    map2(c("/dataset", rep(NA_character_, 4)),
         function(x, q){
           resp_body_string(x) |>
             fparse(query = if (is.na(q)) NULL else q) |>
             as_fibble()
         }) |>
    set_names(c("care", "prov", "open", "caid", "hgov"))


  list(
    care = clog_care(x),
    prov = clog_prov(x),
    open = clog_open(x),
    caid = clog_caid(x),
    hgov = clog_hgov(x)
  )
}

#' @autoglobal
#' @noRd
clog_care <- function(x) {

  cols <- c("title", "clog", "year", "format", "description", "modified", "temporal", "identifier", "download", "resources")

  d <- get_elem(x$care, "distribution", DF.as.list = TRUE) |>
    rowbind(fill = TRUE) |>
    fcompute(
      year       = extract_year(title),
      title      = gremove(title, " : [0-9]{4}-[0-9]{2}-[0-9]{2}([0-9A-Za-z]{1,3})?$"),
      format     = iif(!is.na(description), description, format, nThread = 4L),
      modified   = as_date(modified),
      temporal   = fmt_temporal(temporal),
      identifier = accessURL,
      download   = lag_(downloadURL, n = -1L),
      resources  = resourcesAPI) |>
    roworder(title, -year) |>
    as_fibble()

  d <- sset(d, row_na_counts(d) < 4)

  x <- mtt(x$care,
    modified    = as_date(modified),
    periodicity = fmt_periodicity(accrualPeriodicity),
    contact     = fmt_contactpoint(get_elem(x$care, "contactPoint")),
    references  = delist(references),
    temporal    = fmt_temporal(temporal),
    title       = rm_nonascii(rm_space(title)),
    description = rm_quotes(rm_nonascii(description)),
    description = description |>
      gremove("Note: This full dataset contains more records than most spreadsheet programs can handle, which will result in an incomplete load of data. Use of a database or statistical software is required.$") |>
      gremove("^ATTENTION USERSSome Providers Opt-Out Status may end early due to COVID 19 waivers. Please contact your respective MAC for further information. For more information on the opt-out process, see Manage Your Enrollment or view the FAQ section below. ") |>
      gremove("On November 17, 2023, CMS published in the Federal Register a final rule titled, .+Medicare and Medicaid Programs; Disclosures of Ownership and Additional Disclosable Parties Information for Skilled Nursing Facilities and Nursing Facilities; Medicare Providers.+ and Suppliers.+ Disclosure of Private Equity Companies and Real Estate Investment Trusts.+ .+88 FR 80141.+. This final rule implements parts of section 1124.+c.+ \\n\\n.+\\n\\n.+") |>
      gremove("\\n\\n.+\\n\\n")) |>
    slt(title, description, modified, periodicity, temporal, contact, identifier, dictionary = describedBy, site = landingPage, references) |>
    join_on_title(sbt(d, format == "latest", title, download, resources)) |>
    roworder(title)

  d <- sbt(d, title %!in_% care_types("single") & format != "latest", -format) |>
    roworder(title, -year) |>
    fnest(by = c("title")) |>
    rnm(endpoints = data) |>
    join_on_title(slt(x, title, description, periodicity, contact, dictionary, site, references)) |>
    colorder(endpoints, pos = "end")

  list(end = mtt(x, clog = "care", api = "end") |> colorder(clog, api),
       tmp = mtt(d, clog = "care", api = "tmp") |> colorder(clog, api))
}

#' @autoglobal
#' @noRd
clog_prov <- function(x) {

  list(
    end = mtt(
      x$prov,
      clog        = "prov",
      api         = "end",
      title       = rm_nonascii(title),
      dictionary  = paste0("https://data.cms.gov/provider-data/dataset/", identifier, "#data-dictionary"),
      identifier  = paste0("https://data.cms.gov/provider-data/api/1/datastore/query/", identifier, "/0"),
      issued      = as_date(issued),
      modified    = as_date(modified),
      released    = as_date(released),
      group       = flatten_column(theme),
      description = gremove(description, "\n"),
      download    = get_elem(x$prov, "distribution") |> get_elem("^downloadURL", regex = TRUE, DF.as.list = TRUE) |> delist(),
      contact     = get_elem(x$prov, "contactPoint") |> fmt_contactpoint()) |>
      sbt(group %!=% "Physician office visit costs", clog, api, title, group, description, issued, modified, released, identifier, contact, download, site = landingPage, dictionary) |>
      roworder(group, title)
  )
}

#' @autoglobal
#' @noRd
clog_open <- function(x) {

  x <- mtt(x$open,
      identifier  = paste0("https://openpaymentsdata.cms.gov/api/1/datastore/query/", identifier, "/0"),
      modified    = as_date(modified),
      year        = unlist(x$open$keyword, use.names = FALSE),
      year        = iif(year == "all years" | title == "Provider profile ID mapping table", "All", year, nThread = 4L),
      title       = toTitleCase(rm_nonascii(title)),
      contact     = get_elem(x$open, "contactPoint") |> fmt_contactpoint(),
      description = rm_quotes(description) |>
        rm_nonascii() |>
        greplace("\r\n", " ") |>
        gremove("<p><strong>NOTE: </strong>This is a very large file and, depending on your network characteristics and software, may take a long time to download or fail to download. Additionally, the number of rows in the file may be larger than the maximum rows your version of <a href=https://support.microsoft.com/en-us/office/excel-specifications-and-limits-1672b34d-7043-467e-8e27-269d656771c3>Microsoft Excel</a> supports. If you cant download the file, we recommend engaging your IT support staff. If you are able to download the file but are unable to open it in MS Excel or get a message that the data has been truncated, we recommend trying alternative programs such as MS Access, Universal Viewer, Editpad or any other software your organization has available for large datasets.</p>$") |>
        rm_space(),
      download    = get_elem(x$open, "distribution", DF.as.list = TRUE) |>
        get_elem("downloadURL", DF.as.list = TRUE) |>
        unlist(use.names = FALSE)) |>
    slt(year, title, description, modified, identifier, contact, download)

  list(
    end = sbt(x, year %==% "All", -year) |>
      mtt(clog = "open",
          api = "end") |>
      colorder(clog, api) |>
      roworder(title),
    tmp = sbt(x, year %!=% "All") |>
      mtt(clog = "open",
          api = "tmp",
          year = as.integer(year),
          title = gremove(title, "^[0-9]{4} "),
          description = nswitch(
            title,
            "General Payment Data"   , "All general (non-research, non-ownership related) payments from the program year",
            "Ownership Payment Data" , "All ownership and investment payments from the program year",
            "Research Payment Data"  , "All research-related payments from the program year",
            default = description,
            nThread = 4L)
          ) |>
      colorder(clog, api) |>
      roworder(title, -year) |>
      fnest(by = c("clog", "api", "title", "description", "modified")) |>
      rnm(endpoints = data))
}

#' @autoglobal
#' @noRd
clog_caid <- function(x) {

  cols <- c("title", "identifier", "description", "periodicity", "modified", "contact")

  w <- get_elem(x$caid, "distribution", DF.as.list = TRUE) |>
    get_elem("^title$|^downloadURL$", DF.as.list = TRUE, regex = TRUE)

  dl <- fibble(
    title = map(w, "title") |> unlist(use.names = FALSE),
    download = map(w, "downloadURL") |> unlist(use.names = FALSE),
    ext = path_ext(download)) |>
    fcount(title, add = TRUE) |>
    mtt(title = ifelse_(title == "CSV", NA_character_, title),
        N = ifelse_(is.na(title), NA_integer_, N)) |>
    roworder(title)

  x <- mtt(x$caid,
      identifier  = paste0("https://data.medicaid.gov/api/1/datastore/query/", identifier, "/0"),
      modified    = as_date(modified),
      periodicity = fmt_periodicity(accrualPeriodicity),
      contact     = get_elem(x$caid, "contactPoint") |> fmt_contactpoint(),
      title       = gremove(title, "^ ") |> rm_nonascii() |> rm_space(),
      description = iif(description == "Dataset.", NA_character_, description),
      description = rm_nonascii(description) |>
        rm_quotes() |>
        greplace("\r\n", " ") |>
        rm_space()) |>
    ss(j = cols)

  # x <- list(x, rowbind(ss(dl, which_(dl$N == 1)) |> fcount(title, sort = TRUE, decreasing = TRUE),
  #   ss(dl, which_(dl$ext == "csv" & dl$N > 1), 1:2)), ss(dl, which_(dl$ext != "csv"), 1:2) |> fnest(by = "title") |> rnm(resources = data)) |>
  #   reduce(join_on_title) |> roworder(title)

  ptn <- paste0(
    "State Drug Utilization Data [0-9]{4}|",
    "NADAC \\(National Average Drug Acquisition Cost\\)|",
    "^[0-9]{4} Child and Adult Health Care Quality|",
    "^[0-9]{4} Managed Care Programs by State$|",
    "^Pricing Comparison for Blood Disorder Treatments|",
    "^Child and Adult Health Care Quality Measures|",
    "^Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program [0-9]{2}")

  list(
    end = ss_title(x, ptn, n = TRUE) |>
      ss_title("CoreS|Scorecard|Auto", n = TRUE) |>
      mtt(clog = "caid",
          api = "end") |>
      colorder(clog, api),
    tmp = ss_title(x, ptn) |> ss_title("CoreS|Scorecard|Auto", n = TRUE) |>
      mtt(clog = "caid",
          api = "end",
          year = extract_year(title),
          title = nif(
      gdetect(title, "Child and Adult Health Care Quality Measures")       , "Child and Adult Health Care Quality Measures",
      gdetect(title, "[0-9]{4} Manage")                                    , "Managed Care Programs by State",
      gdetect(title, "NADAC \\(National Average Drug Acquisition Cost\\)") , "NADAC",
      gdetect(title, "State Drug Utilization Data")                        , "State Drug Utilization Data",
      gdetect(title, "Pricing Comparison")                                 , "Pricing Comparison for Blood Disorder Treatments",
      gdetect(title, "Product Data for Newly Reported")                    , "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program",
      default = title),
      description = iif(title == "Child and Adult Health Care Quality Measures",
                        "Performance rates on frequently reported health care quality measures in the CMS Medicaid/CHIP Child and Adult Core Sets. Dataset contains both child and adult measures.",
                        description,
                        nThread = 4L)
      ) |>
      roworder(title, year) |>
      ffill(description, periodicity) |>
      slt(clog, api, year, title, description, periodicity, modified, identifier) |>
      roworder(title, -year) |>
      fnest(by = c("clog", "api", "title", "description", "periodicity")) |>
      rnm(endpoints = data),
    download = dl
    )
}

#' @autoglobal
#' @noRd
clog_hgov <- function(x) {

  cols <- c("title", "identifier", "description", "periodicity", "issued", "modified", "contact")

  d <- cheapr::col_c(
    title = get_elem(x$hgov, "title"),
    distribution = get_elem(x$hgov, "distribution")) |>
    as_fibble()|>
    fastplyr::f_mutate(
      n = purrr::map_int(distribution, \(x) nrow(x)),
      id = fastplyr::f_consecutive_id(title))

  d$distribution <- set_names(d$distribution, d$id)

  d <- imap(d$distribution, function(x, idx) {
    glue("d$distribution$`{idx}`[['downloadURL']]") |>
      parse_expr() |>
      eval_bare()
    }) |>
    unlist() |>
    set_names(strtrim, 3) |>
    fastplyr::f_enframe(name = "id", value = "download") |>
    mtt(id = as.integer(id),
        ext = path_ext(download)) |>
    join(d, on = "id", verbose = 0, multiple = TRUE) |>
    slt(-distribution) |>
    colorder(id, title, ext, download, n) |>
    fcount(id, add = TRUE)


  x <- mtt(x$hgov,
      identifier  = paste0("https://data.healthcare.gov/api/1/datastore/query/", identifier, "/0"),
      title       = rm_nonascii(title) |> rm_space(),
      description = rm_nonascii(description) |> rm_quotes() |> gremove("<a href=|>|target=_blank rel=noopener noreferrer|</a|<br|@\\s") |> greplace("Dataset.", NA_character_),
      modified    = as_date(modified),
      issued      = as_date(issued),
      periodicity = fmt_periodicity(accrualPeriodicity),
      contact     = fmt_contactpoint(x$hgov$contactPoint)) |>
    slt(cols)

  x <- list(
    x,
    sbt(d, ext == "csv", title, download),
    sbt(d, ext != "csv", title, resources = download)) |>
    reduce(join_on_title)

  qhp <- ss_title(x, "QHP|SHOP")

  temporal <- ss_title(x, "[2][0-9]{3}|\\sPUF") |>
    sbt(title %!in_% get_elem(qhp, "title")) |>
    ss_title("Qualifying|QHP Landscape Health Plan Business Rule Variables", n = TRUE) |>
    mtt(
      periodicity = "Annually [R/P1Y]",
      year  = extract_year(title),
      title = gremove(title, "^[2][0-9]{3}\\s") |>
        gremove("\\s\\s?[-]?\\s?[2][0-9]{3}$") |>
        gremove("\\s\\s?[-]?\\s?PY[2][0-9]{3}$") |>
        gremove("[RP]Y\\s?[2][0-9]{3}\\s") |>
        gremove("\\s[0-9]{8}$") |>
        gremove("\\sSocrata|\\sZip\\sFile$"),
      title = nif(
        gdetect(title, "^Benefits\\sCost")                    , "Benefits and Cost Sharing PUF",
        gdetect(title, "^Plan\\sCrosswalk\\sPUF")             , "Plan ID Crosswalk PUF",
        gdetect(title, "Transparency [Ii]n Coverage PUF")     , "Transparency in Coverage PUF",
        default = title),
      title = rm_space(title) |> greplace("/\\s", "/"),
      description = nswitch(title,
        "Benefits and Cost Sharing PUF" , "The Benefits and Cost Sharing PUF (BenCS-PUF) is one of the files that comprise the Health Insurance Exchange Public Use Files. The BenCS-PUF contains plan variant-level data on essential health benefits, coverage limits, and cost sharing for each QHP and SADP.",
        "Business Rules PUF"            , "The Business Rules PUF (BR-PUF) is one of the files that comprise the Health Insurance Exchange Public Use Files. The BR-PUF contains plan-level data on rating business rules, such as maximum age for a dependent and allowed dependent relationships.",
        "MLR Dataset"                   , "This file contains Medical Loss Ratio data for the Reporting Year, including the issuers MLR, the MLR standard, and the average rebate per family by state and market.",
        "Machine Readable PUF"          , "The Machine Readable PUF (MR-PUF) is one of the files that comprise the Health Insurance Exchange Public Use Files. The MR-PUF contains issuer-level URL locations for machine-readable network provider and formulary information.",
        "Network PUF"                   , "The Network PUF (Ntwrk-PUF) is one of the files that comprise the Health Insurance Exchange Public Use Files. The Ntwrk-PUF contains issuer-level data identifying provider network URLs.",
        "Plan Attributes PUF"           , "The Plan Attributes PUF (Plan-PUF) is one of the files that comprise the Health Insurance Exchange Public Use Files. The Plan-PUF contains plan variant-level data on maximum out of pocket payments, deductibles, health savings account (HSA) eligibility, and other plan attributes.",
        "Plan ID Crosswalk PUF"         , "The Plan ID Crosswalk PUF (CW-PUF) is one of the files that comprise the Health Insurance Exchange Public Use Files. The purpose of the CW-PUF is to map QHPs and SADPs offered through the Exchanges during the previous plan year to plans that will be offered through the Exchanges in the current plan year.",
        "Quality PUF"                   , "The Quality PUF contains yearly quality ratings data for eligible Qualified Health Plans in states on HealthCare.gov.",
        "Rate PUF"                      , "The Rate PUF (Rate-PUF) is one of the files that comprise the Health Insurance Exchange Public Use Files. The Rate-PUF contains plan-level data on rates based on an eligible subscribers age, tobacco use, and geographic location; and family-tier rates.",
        "Service Area PUF"              , "The Service Area PUF (SA-PUF) is one of the files that comprise the Health Insurance Exchange Public Use Files. The SA-PUF contains issuer-level data on geographic service areas including state, county, and zip code.",
        "Transparency in Coverage PUF"  , "The Transparency in Coverage PUF (TC-PUF) is one of the files that comprise the Health Insurance Exchange Public Use Files. The PUF contains data on issuer and plan-level claims, appeals, and active URL data.",
        default = description,
        nThread = 4L)
      ) |>
    ss_title("\\.zip$|Excel$", n = TRUE) |>
    colorder(year) |>
    roworder(title, -year)


  qhp <- qhp |>
    ss_title("Excel|[Zz]ip|Instructions|\\.zip$", n = TRUE) |>
    mtt(
      description = iif(is.na(description), title, description, nThread = 4L),
      year = extract_year(title),
      year = ifelse_(is.na(year), paste0("20", gextract(title, "[0-9]{2}")), year),
      year = iif(is.na(year), substr(modified, 1, 4), year, nThread = 4L),
    title = gremove(title, "^[2][0-9]{3}\\s") |>
      gremove("\\s\\s?[-]?\\s?[2][0-9]{3}$") |>
      gremove("\\s\\s?[-]?\\s?PY[2][0-9]{3}$") |>
      gremove("[RP]Y\\s?[2][0-9]{3}\\s") |>
      gremove("[RP]Y\\s?[1][0-9]\\s") |>
      gremove("\\s[0-9]{8}$") |>
      rm_space() |>
      greplace("/\\s", "/"),
    title = nif(
      gdetect(title, "QHP\\sDent[-]\\sIndi[-]\\s")          , "QHP Landscape Individual Market Dental",
      gdetect(title, "QHP\\sMedi[-]\\sIndi[-]\\s")          , "QHP Landscape Individual Market Medical",
      gdetect(title, "QHP\\sMedi[-]\\sSHOP[-]\\s")          , "QHP Landscape SHOP Market Medical",
      gdetect(title, "QHP\\sMedical\\s[-]\\sSHOP\\s[-]\\s") , "QHP Landscape SHOP Market Medical",
      gdetect(title, "QHP\\sDent[-]\\sSHOP[-]\\s")          , "QHP Landscape SHOP Market Dental",
      gdetect(title, "QHP Landscape Individual Dental")     , "QHP Landscape Individual Market Dental",
      gdetect(title, "QHP Landscape Individual Medical")    , "QHP Landscape Individual Market Medical",
      gdetect(title, "QHP Landscape Medical SHOP")          , "QHP Landscape SHOP Market Medical",
      gdetect(title, "QHP Landscape Dental SHOP")           , "QHP Landscape SHOP Market Dental",
      default = title)
    ) |>
    colorder(year) |>
    roworder(title, -year)

  list(
    end = rowbind(
      ss_title(x, "[2][0-9]{3}|QHP|SHOP|\\sPUF", n = TRUE),
      ss_title(x, "Qualifying|QHP Landscape Health Plan Business Rule Variables")) |>
      mtt(clog  = "hgov",
          api   = "end",
          title = greplace(title, "/\\s", "/") |>
            greplace("Qualifying Health Plan", "QHP") |>
            str_look_remove("County,", "ahead") |>
            gremove("2015 ")) |>
      colorder(clog, api),
    tmp = rowbind(
      temporal,
      ss_title(qhp, "^QHP Landscape [HINO][IDMVR]|^QHP Landscape Health Plan Business Rule Variables", n = TRUE)) |>
      mtt(clog = "hgov", api = "tmp") |>
      colorder(clog, api) |>
      fnest(by = c("clog", "api", "title")) |>
      rnm(endpoints = data))
    # qhp = subset_detect(qhp, title, "^QHP Landscape [HINO][IDMVR]|^QHP Landscape Health Plan Business Rule Variables", n = TRUE) |> mtt(api = "HealthcareGov [Temporal]") |>
    # colorder(api) |> f_nest_by(.by = c(api, title)) |> f_ungroup() |> rnm(endpoints = data),
    # states = subset_detect(qhp, title, "^QHP Landscape [HINO][IDMVR]")
}

the         <- new.env(parent = emptyenv())
the$catalog <- catalogs()

# m <- fastmap::fastmap()
# m$mset(.list = providertwo:::catalogs())
# m$get("care")
# m$keys()
# m$size()
# m$as_list()
# m$reset()

#' @autoglobal
#' @noRd
reset_catalog <- function() {
  old         <- the$catalog
  the$catalog <- catalogs()
  invisible(old)
}
