#' @include utils_misc.R
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
#' @examplesIf interactive()
#' catalogs()
#' @autoglobal
#' @keywords internal
#' @export
catalogs <- function() {
  base_url <- c(
    care = "https://data.cms.gov/data.json",
    prov = "https://data.cms.gov/provider-data/api/1/metastore/schemas/dataset/items",
    open = "https://openpaymentsdata.cms.gov/api/1/metastore/schemas/dataset/items",
    caid = "https://data.medicaid.gov/api/1/metastore/schemas/dataset/items",
    hgov = "https://data.healthcare.gov/api/1/metastore/schemas/dataset/items"
  )

  x <- base_url |>
    purrr::map(httr2::request) |>
    httr2::req_perform_parallel(on_error = "continue") |>
    httr2::resps_successes() |>
    purrr::map2(c("/dataset", rep(NA_character_, 4)), function(x, q) {
      httr2::resp_body_string(x) |>
        RcppSimdJson::fparse(query = if (is.na(q)) NULL else q) |>
        fastplyr::as_tbl()
    }) |>
    rlang::set_names(rlang::names2(base_url))

  list(clog_care(x), clog_prov(x), clog_open(x), clog_caid(x), clog_hgov(x)) |>
    rlang::set_names(rlang::names2(base_url))
}

#' @autoglobal
#' @noRd
get_care <- function(x) {
  collapse::get_elem(x, "care")
}

#' @autoglobal
#' @noRd
get_prov <- function(x) {
  collapse::get_elem(x, "prov")
}

#' @autoglobal
#' @noRd
get_caid <- function(x) {
  collapse::get_elem(x, "caid")
}

#' @autoglobal
#' @noRd
get_open <- function(x) {
  collapse::get_elem(x, "open")
}

#' @autoglobal
#' @noRd
get_hgov <- function(x) {
  collapse::get_elem(x, "hgov")
}

#' @autoglobal
#' @noRd
get_distribution <- function(x, ...) {
  collapse::get_elem(x, "distribution", ...)
}

#' @autoglobal
#' @noRd
clog_care <- function(x) {

  dist <- get_care(x) |>
    get_distribution(DF.as.list = TRUE) |>
    collapse::rowbind(fill = TRUE) |>
    collapse::fcompute(
      year       = extract_year(title),
      title      = gremove(title, " : [0-9]{4}-[0-9]{2}-[0-9]{2}([0-9A-Za-z]{1,3})?$"),
      format     = kit::iif(!is.na(description), description, format, nThread = 4L),
      modified   = as_date(modified),
      temporal   = fmt_temporal(temporal),
      identifier = accessURL,
      download   = cheapr::lag_(downloadURL, n = -1L),
      resources  = resourcesAPI) |>
    collapse::roworder(title, -year) |>
    fastplyr::as_tbl()

  dist <- cheapr::sset(dist, cheapr::which_(cheapr::row_na_counts(dist) < 3L))

  base <- get_care(x) |>
    collapse::mtt(
      uuid        = uuid_from_url(identifier),
      modified    = as_date(modified),
      periodicity = fmt_periodicity(accrualPeriodicity),
      contact     = fmt_contactpoint(get_care(x)),
      references  = unlist(references, use.names = FALSE),
      temporal    = fmt_temporal(temporal),
      title       = clean_title(title),
      description = clean_title(description),
      dictionary  = describedBy,
      site        = landingPage,
      .keep       = c("identifier", "references")
    ) |>
    join_on_title(collapse::sbt(
      dist,
      format %==% "latest",
      c("title", "download", "resources"))) |>
    collapse::roworder(title) |>
    collapse::colorder(title, description)

  base <- cheapr::sset(base, cheapr::which_(cheapr::row_na_counts(base) < 3L))

  dist <- collapse::sbt(dist, format %!=% "latest", -format) |>
    collapse::roworder(title, -year) |>
    collapse::gby(title, return.groups = FALSE) |>
    collapse::mtt(download_only = collapse::allNA(identifier)) |>
    fnest(by = c("title", "download_only")) |>
    join_on_title(
      collapse::slt(
        base,
      c(
        "title",
        "description",
        "periodicity",
        "contact",
        "dictionary",
        "site",
        "references"
      )
    )) |>
    collapse::colorder(endpoints, pos = "end") |>
    collapse::rsplit(~ download_only, use.names = FALSE) |>
    rlang::set_names(c("tmp", "dwn"))

  list(
    current = base,
    temporal = dist$tmp,
    download_only = dist$dwn
    )
}

#' @autoglobal
#' @noRd
clog_prov <- function(x) {
  list(
    current = get_prov(x) |>
      collapse::mtt(
        uuid        = identifier,
        title       = clean_title(title),
        dictionary  = paste0(
        "https://data.cms.gov/provider-data/dataset/",
        identifier,
        "#data-dictionary"
      ),
      identifier  = paste0(
        "https://data.cms.gov/provider-data/api/1/datastore/query/",
        identifier, "/0"
      ),
      issued      = as_date(issued),
      modified    = as_date(modified),
      released    = as_date(released),
      next_update = as_date(nextUpdateDate),
      group       = purrr::map_chr(theme, function(x) toString(unlist(x, use.names = FALSE))),
      description = clean_title(description),
      download    = prov_download(x),
      contact     = fmt_contactpoint(get_prov(x)),
      site        = landingPage
    ) |>
      collapse::sbt(
        group %!=% "Physician office visit costs",
        c(
          "title",
          "group",
          "description",
          "issued",
          "modified",
          "released",
          "next_update",
          "uuid",
          "identifier",
          "contact",
          "download",
          "site",
          "dictionary"
        )
      ) |>
      collapse::roworder(group, title)
  )
}

#' @autoglobal
#' @noRd
clog_open <- function(x) {

  x <- get_open(x) |>
    collapse::mtt(
      uuid = identifier,
      identifier = paste0(
      "https://openpaymentsdata.cms.gov/api/1/datastore/query/",
      identifier, "/0"
    ),
    modified = as_date(modified),
    year = unlist(get_open(x)$keyword, use.names = FALSE),
    year = kit::iif(
      year == "all years" | title == "Provider profile ID mapping table",
      "All",
      year,
      nThread = 4L
    ),
    title = clean_title(title),
    contact = fmt_contactpoint(get_open(x)),
    description = clean_title(description),
    download = open_download(x)
  ) |>
    collapse::slt(c(
      "year",
      "title",
      "description",
      "modified",
      "identifier",
      "uuid",
      "contact",
      "download"
    ))

  list(
    current = collapse::sbt(x, year %==% "All", -year) |> collapse::roworder(title),
    temporal = collapse::sbt(x, year %!=% "All") |>
      collapse::mtt(
        year = as.integer(year),
        title = gremove(title, "^[0-9]{4} "),
        description = kit::nswitch(
          title,
          "General Payment Data", "All general (non-research, non-ownership related) payments from the program year",
          "Ownership Payment Data", "All ownership and investment payments from the program year",
          "Research Payment Data", "All research-related payments from the program year",
          default = description,
          nThread = 4L)) |>
      collapse::roworder(title, -year) |>
      fnest(by = c("title"))
  )
}

#' @autoglobal
#' @noRd
clog_caid <- function(x) {

  cols <- c(
    "title",
    "uuid",
    "identifier",
    "description",
    "periodicity",
    "modified",
    "contact",
    "download"
  )

  base <- get_caid(x) |>
    collapse::mtt(
      uuid = identifier,
      identifier = paste0(
        "https://data.medicaid.gov/api/1/datastore/query/",
        identifier,
        "/0"
      ),
      modified = as_date(modified),
      periodicity = fmt_periodicity(accrualPeriodicity),
      contact = fmt_contactpoint(get_caid(x)),
      title = clean_title(title),
      description = kit::iif(description == "Dataset.", NA_character_, description, nThread = 4L),
      description = clean_title(description)
    ) |>
    collapse::sbt(
      grep(
        "test|coreset|scorecard|category_tiles|auto",
        title,
        ignore.case = TRUE,
        perl = TRUE,
        invert = TRUE
      )
    ) |>
    join_on_title(caid_download(x)) |>
    collapse::slt(cols)

  ptn <- paste(
    "State Drug Utilization Data [0-9]{4}",
    "NADAC \\(National Average Drug Acquisition Cost\\) 20[0-9]{2}",
    "^20[0-9]{2} Child and Adult",
    "^20[0-9]{2} Managed Care",
    "^Pricing Comparison for Blood Disorder Treatments",
    "^Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program [0-9]{2}",
    sep = "|"
  )

  list(
    current = collapse::sbt(
      base,
      stringi::stri_detect_regex(title, pattern = ptn, negate = TRUE)) |>
      collapse::roworder(title),
    temporal = collapse::sbt(
      base,
      stringi::stri_detect_regex(title, pattern = ptn)) |>
      collapse::roworder(title) |>
      collapse::mtt(
        year = extract_year(title),
        title = caid_title(title),
        description = kit::iif(
          title == "Child and Adult Health Care Quality Measures",
          paste0(
            "Performance rates on frequently reported health ",
            "care quality measures in the CMS Medicaid/CHIP Child ",
            "and Adult Core Sets. Dataset contains both child and adult measures."
            ),
          description,
          nThread = 4L
          )
        ) |>
      collapse::roworder(title, year) |>
      ffill(description, periodicity) |>
      collapse::slt(
        "year",
        "title",
        "description",
        "periodicity",
        "modified",
        "identifier",
        "download"
      ) |>
      collapse::roworder(title, -year) |>
      fnest(by = c("title", "description", "periodicity"))
  )
}

#' @autoglobal
#' @noRd
caid_title <- function(x) {
  kit::nif(
    gdetect(x, "Child and Adult Health Care Quality Measures"), "Child and Adult Health Care Quality Measures",
    gdetect(x, "2[0-9]{3} Manage"), "Managed Care Programs by State",
    gdetect(x, "NADAC \\(National Average Drug Acquisition Cost\\)"), "NADAC",
    gdetect(x, "State Drug Utilization Data"), "State Drug Utilization Data",
    gdetect(x, "Pricing Comparison"), "Pricing Comparison for Blood Disorder Treatments",
    gdetect(x, "Product Data for Newly Reported"), "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program",
    default = x
  )
}

#' @autoglobal
#' @noRd
clog_hgov <- function(x) {

  d <- cheapr::col_c(
    title = collapse::get_elem(get_hgov(x), "title"),
    distribution = get_distribution(get_hgov(x))) |>
    fastplyr::as_tbl() |>
    fastplyr::f_mutate(
      n = purrr::map_int(distribution, function(x) nrow(x)),
      id = fastplyr::f_consecutive_id(title))

  d$distribution <- rlang::set_names(d$distribution, d$id)

  d <- purrr::imap(d$distribution, function(x, idx) {
    glue::glue("d$distribution$`{idx}`[['downloadURL']]") |>
      rlang::parse_expr() |>
      rlang::eval_bare()
  }) |>
    unlist() |>
    rlang::set_names(strtrim, 3) |>
    fastplyr::f_enframe(name = "id", value = "download") |>
    collapse::mtt(id = as.integer(id), ext = fs::path_ext(download)) |>
    join_on(d, on = "id") |>
    collapse::slt(-distribution) |>
    collapse::colorder(id, title, ext, download, n) |>
    collapse::fcount(id, add = TRUE)


  x <- get_hgov(x) |>
    collapse::mtt(
      uuid = identifier,
      identifier = paste0(
      "https://data.healthcare.gov/api/1/datastore/query/",
      identifier,
      "/0"
    ),
    title = clean_title(title),
    description = clean_title(description) |>
      gremove("<a href=|>|target=_blank rel=noopener noreferrer|</a|<br|@\\s") |>
      greplace("Dataset.", NA_character_),
    modified    = as_date(modified),
    issued      = as_date(issued),
    periodicity = fmt_periodicity(accrualPeriodicity),
    contact     = fmt_contactpoint(get_hgov(x))
  ) |>
    collapse::slt(
      c(
        "title",
        "uuid",
        "identifier",
        "description",
        "periodicity",
        "issued",
        "modified",
        "contact"
      )
    )

  x <- list(x,
    collapse::sbt(d, ext %==% "csv", "title", "download"),
    collapse::sbt(d, ext %!=% "csv", "title", resources = "download")) |>
    purrr::reduce(join_on_title)

  qhp <- ss_title(x, "QHP|SHOP")

  temporal <- ss_title(x, "[2][0-9]{3}|\\sPUF") |>
    collapse::sbt(title %!iin% collapse::get_elem(qhp, "title")) |>
    ss_title("Qualifying|QHP Landscape Health Plan Business Rule Variables", n = TRUE) |>
    collapse::mtt(
      periodicity = "Annually [R/P1Y]",
      year  = extract_year(title),
      title = gremove(title, "^[2][0-9]{3}\\s") |>
        gremove("\\s\\s?[-]?\\s?[2][0-9]{3}$") |>
        gremove("\\s\\s?[-]?\\s?PY[2][0-9]{3}$") |>
        gremove("[RP]Y\\s?[2][0-9]{3}\\s") |>
        gremove("\\s[0-9]{8}$") |>
        gremove("\\sSocrata|\\sZip\\sFile$"),
      title = kit::nif(
        gdetect(title, "^Benefits\\sCost")                    , "Benefits and Cost Sharing PUF",
        gdetect(title, "^Plan\\sCrosswalk\\sPUF")             , "Plan ID Crosswalk PUF",
        gdetect(title, "Transparency [Ii]n Coverage PUF")     , "Transparency in Coverage PUF",
        default = title),
      title = clean_title(title) |> greplace("/\\s", "/"),
      description = kit::nswitch(title,
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
        nThread = 4L)) |>
    ss_title("\\.zip$|Excel$", n = TRUE) |>
    collapse::colorder(year) |>
    collapse::roworder(title, -year)


  qhp <- qhp |>
    ss_title("Excel|[Zz]ip|Instructions|\\.zip$", n = TRUE) |>
    collapse::mtt(
      description = kit::iif(is.na(description), title, description, nThread = 4L),
      year = extract_year(title),
      year = ifelse(is.na(year), paste0("20", gextract(title, "[0-9]{2}")), year),
      year = kit::iif(is.na(year), substr(modified, 1, 4), year, nThread = 4L),
      title = gremove(title, "^[2][0-9]{3}\\s") |>
        gremove("\\s\\s?[-]?\\s?[2][0-9]{3}$") |>
        gremove("\\s\\s?[-]?\\s?PY[2][0-9]{3}$") |>
        gremove("[RP]Y\\s?[2][0-9]{3}\\s") |>
        gremove("[RP]Y\\s?[1][0-9]\\s") |>
        gremove("\\s[0-9]{8}$") |>
        rm_space() |>
        greplace("/\\s", "/"),
      title = kit::nif(
        gdetect(title, "QHP\\sDent[-]\\sIndi[-]\\s"),
        "QHP Landscape Individual Market Dental",
        gdetect(title, "QHP\\sMedi[-]\\sIndi[-]\\s"),
        "QHP Landscape Individual Market Medical",
        gdetect(title, "QHP\\sMedi[-]\\sSHOP[-]\\s"),
        "QHP Landscape SHOP Market Medical",
        gdetect(title, "QHP\\sMedical\\s[-]\\sSHOP\\s[-]\\s"),
        "QHP Landscape SHOP Market Medical",
        gdetect(title, "QHP\\sDent[-]\\sSHOP[-]\\s"),
        "QHP Landscape SHOP Market Dental",
        gdetect(title, "QHP Landscape Individual Dental"),
        "QHP Landscape Individual Market Dental",
        gdetect(title, "QHP Landscape Individual Medical"),
        "QHP Landscape Individual Market Medical",
        gdetect(title, "QHP Landscape Medical SHOP"),
        "QHP Landscape SHOP Market Medical",
        gdetect(title, "QHP Landscape Dental SHOP"),
        "QHP Landscape SHOP Market Dental",
        default = title
      )
    ) |>
    collapse::colorder(year) |>
    collapse::roworder(title, -year)

  list(
    current = collapse::rowbind(
      ss_title(x, "[2][0-9]{3}|QHP|SHOP|\\sPUF", n = TRUE),
      ss_title(x, "Qualifying|QHP Landscape Health Plan Business Rule Variables")
    ) |>
      collapse::mtt(
        title = greplace(title, "/\\s", "/") |>
          greplace("Qualifying Health Plan", "QHP") |>
          str_look_remove("County,", "ahead") |>
          gremove("2015 ")
      ),
    temporal = collapse::rowbind(
      temporal,
      ss_title(
        qhp,
        "^QHP Landscape [HINO][IDMVR]|^QHP Landscape Health Plan Business Rule Variables",
        n = TRUE
      )
    ) |>
      fnest(by = "title")
  )
}

#' @autoglobal
#' @noRd
fmt_contactpoint <- function(x) {
  x <- collapse::get_elem(x, "contactPoint")

  glue::glue("{rlang::names2(x)} ({x})",
    x = collapse::get_elem(x, "^has", regex = TRUE) |>
      unlist(use.names = FALSE) |>
      rlang::set_names(
        collapse::get_elem(x, "fn") |>
          unlist(use.names = FALSE))) |>
    as.character()
}

#' @autoglobal
#' @noRd
fmt_temporal <- function(x) {
  greplace(x, "/", paste0(" ", cli::symbol$bullet, " "))
}

#' @autoglobal
#' @noRd
caid_download <- function(x) {

  to_string  <- \(x)    purrr::map_chr(x, \(i) toString(unlist(i, use.names = FALSE), width = NULL))
  to_string2 <- \(x, e) purrr::map_chr(x, \(i) toString(unlist(collapse::get_elem(x, e), use.names = FALSE), width = NULL))

  s <- get_caid(x) |>
    collapse::slt(title, distribution) |>
    collapse::sbt(grep("test|coreset|scorecard|category_tiles|auto", title, ignore.case = TRUE, perl = TRUE, invert = TRUE)) |>
    collapse::roworder(title) |>
    collapse::mtt(
      distribution = purrr::map(distribution, \(x) collapse::get_elem(x, "^title$|^downloadURL$", DF.as.list = TRUE, regex = TRUE)),
      is_chr = purrr::map_lgl(distribution, \(x) is.character(x))) |>
    collapse::rsplit(~ is_chr)

  purrr::imap(s, function(x, i) {
    switch(i,
      `TRUE`  = collapse::slt(collapse::mtt(x, download = to_string(distribution)), title, download),
      `FALSE` = collapse::mtt(x,
        name     = to_string2(distribution, "title"),
        download = to_string2(distribution, "downloadURL"),
        name     = kit::iif(name == "CSV", title, name, nThread = 4L),
        name     = kit::iif(title == name, NA_character_, name, nThread = 4L)) |>
        collapse::slt(title, download))
  }) |>
    purrr::list_rbind() |>
    collapse::roworder(title)
}

#' @autoglobal
#' @noRd
prov_download <- function(x) {
  get_prov(x) |>
    get_distribution() |>
    collapse::get_elem("^downloadURL", regex = TRUE, DF.as.list = TRUE) |>
    unlist(use.names = FALSE)
}

#' @autoglobal
#' @noRd
open_download <- function(x) {
  get_open(x) |>
    get_distribution(DF.as.list = TRUE) |>
    collapse::get_elem("downloadURL", DF.as.list = TRUE) |>
    unlist(use.names = FALSE)
}
