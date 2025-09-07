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
#' @examples
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
    purrr::map(request) |>
    httr2::req_perform_parallel(on_error = "continue") |>
    httr2::resps_successes() |>
    purrr::map2(c("/dataset", rep(NA_character_, 4)), function(x, q) {
      httr2::resp_body_string(x) |>
        RcppSimdJson::fparse(query = if (is.na(q)) NULL else q) |>
        fastplyr::as_tbl()
    }) |>
    rlang::set_names(rlang::names2(base_url))

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

  distro <- collapse::get_elem(x, "care") |>
    collapse::get_elem("distribution", DF.as.list = TRUE) |>
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

  distro <- cheapr::sset(distro, cheapr::row_na_counts(distro) < 3L)

  base <- collapse::get_elem(x, "care") |>
    collapse::mtt(
    modified    = as_date(modified),
    periodicity = fmt_periodicity(accrualPeriodicity),
    contact     = fmt_contactpoint(collapse::get_elem(x, "care")),
    references  = unlist(references, use.names = FALSE),
    temporal    = fmt_temporal(temporal),
    title       = clean_title(title),
    description = clean_title(description),
    dictionary  = describedBy,
    site        = landingPage,
    .keep       = c("identifier", "references")
  ) |>
    join_on_title(
      collapse::sbt(distro, format %==% "latest", c("title", "download", "resources"))
      ) |>
    collapse::roworder(title) |>
    collapse::colorder(title, description)

  distro <- collapse::sbt(distro, format %!=% "latest", -format) |>
    collapse::roworder(title, -year) |>
    fnest(by = "title") |>
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
    collapse::colorder(endpoints, pos = "end")

  list(current = base, temporal = distro)
}

#' @autoglobal
#' @noRd
clog_prov <- function(x) {
  list(
    current = collapse::get_elem(x, "prov") |>
      collapse::mtt(
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
      download    = collapse::get_elem(x, "prov") |>
        collapse::get_elem("distribution") |>
        collapse::get_elem("^downloadURL", regex = TRUE, DF.as.list = TRUE) |>
        unlist(use.names = FALSE),
      contact     = fmt_contactpoint(x$prov),
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

  x <- collapse::get_elem(x, "open") |>
    collapse::mtt(
    identifier = paste0(
      "https://openpaymentsdata.cms.gov/api/1/datastore/query/",
      identifier, "/0"
    ),
    modified = as_date(modified),
    year = unlist(x$open$keyword, use.names = FALSE),
    year = kit::iif(
      year == "all years" | title == "Provider profile ID mapping table",
      "All",
      year,
      nThread = 4L
    ),
    title = clean_title(title),
    contact = fmt_contactpoint(collapse::get_elem(x, "open")),
    description = clean_title(description),
    download = collapse::get_elem(x, "open") |>
      collapse::get_elem("distribution", DF.as.list = TRUE) |>
      collapse::get_elem("downloadURL", DF.as.list = TRUE) |>
      unlist(use.names = FALSE)
  ) |>
    collapse::slt(c(
      "year",
      "title",
      "description",
      "modified",
      "identifier",
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
      fnest(by = c("title", "description", "modified"))
  )
}

#' @autoglobal
#' @noRd
clog_caid <- function(x) {

  cols <- c(
    "title",
    "identifier",
    "description",
    "periodicity",
    "modified",
    "contact",
    "download"
  )

  base <- collapse::get_elem(x, "caid") |>
    collapse::mtt(
      identifier = paste0(
        "https://data.medicaid.gov/api/1/datastore/query/",
        identifier,
        "/0"
      ),
      modified = as_date(modified),
      periodicity = fmt_periodicity(accrualPeriodicity),
      contact = fmt_contactpoint(collapse::get_elem(x, "caid")),
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
    join_on_title(down_caid(x)) |>
    collapse::slt(cols)

  ptn <- paste0(
    "State Drug Utilization Data 2[0-9]{3}|",
    "NADAC \\(National Average Drug Acquisition Cost\\)|",
    "^2[0-9]{3} Child and Adult Health Care Quality|",
    "^2[0-9]{3} Managed Care Programs by State$|",
    "^Pricing Comparison for Blood Disorder Treatments|",
    "^Child and Adult Health Care Quality Measures|",
    "^Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program [0-9]{2}"
    )

  list(
    current = ss_title(base, ptn, n = TRUE),
    temporal = ss_title(base, ptn) |>
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
    title = collapse::get_elem(x$hgov, "title"),
    distribution = collapse::get_elem(x$hgov, "distribution")
  ) |>
    fastplyr::as_tbl() |>
    fastplyr::f_mutate(
      n = purrr::map_int(distribution, function(x)
        nrow(x)),
      id = fastplyr::f_consecutive_id(title)
    )

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


  x <- collapse::get_elem(x, "hgov") |>
    collapse::mtt(
    identifier = paste0(
      "https://data.healthcare.gov/api/1/datastore/query/",
      identifier,
      "/0"
    ),
    title = rm_nonascii(title) |> rm_space(),
    description = rm_nonascii(description) |>
      rm_quotes() |>
      gremove("<a href=|>|target=_blank rel=noopener noreferrer|</a|<br|@\\s") |>
      greplace("Dataset.", NA_character_),
    modified    = as_date(modified),
    issued      = as_date(issued),
    periodicity = fmt_periodicity(accrualPeriodicity),
    contact     = fmt_contactpoint(x$hgov)
  ) |>
    collapse::slt(
      c(
        "title",
        "identifier",
        "description",
        "periodicity",
        "issued",
        "modified",
        "contact"
      )
    )

  x <- list(
    x,
    collapse::sbt(d, ext == "csv", "title", "download"),
    collapse::sbt(d, ext != "csv", "title", resources = "download")
  ) |>
    purrr::reduce(join_on_title)

  qhp <- ss_title(x, "QHP|SHOP")

  temporal <- ss_title(x, "[2][0-9]{3}|\\sPUF") |>
    collapse::sbt(title %!in_% collapse::get_elem(qhp, "title")) |>
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
      title = rm_space(title) |> greplace("/\\s", "/"),
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
reset_catalog <- function() {
  old         <- the$catalog
  the$catalog <- catalogs()
  invisible(old)
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
down_caid <- function(x) {
  s <- collapse::get_elem(x, "caid") |>
    collapse::slt(title, distribution) |>
    collapse::sbt(
      grep(
        "test|coreset|scorecard|category_tiles|auto",
        title,
        ignore.case = TRUE,
        perl = TRUE,
        invert = TRUE
      )
    ) |>
    collapse::roworder(title) |>
    collapse::mtt(
      distribution = purrr::map(distribution, function(x) {
        collapse::get_elem(x,
                           "^title$|^downloadURL$",
                           DF.as.list = TRUE,
                           regex = TRUE)
      }),
      is_chr = purrr::map_lgl(distribution, function(x) is.character(x))
    ) |>
    collapse::rsplit(~ is_chr)

  purrr::imap(s, function(x, i) {
    switch(
      i,
      `TRUE` = collapse::mtt(x, download = purrr::map_chr(distribution, function(x)
        toString(unlist(x, use.names = FALSE)))) |>
        collapse::slt(title, download),
      `FALSE` = collapse::mtt(
        x,
        name = purrr::map_chr(distribution, function(x)
          toString(
            unlist(collapse::get_elem(x, "title"), use.names = FALSE)
          )),
        download = purrr::map_chr(distribution, \(x) toString(
          unlist(collapse::get_elem(x, "downloadURL"), use.names = FALSE)
        )),
        name = ifelse(name == "CSV", title, name),
        equal = title == name,
        name = ifelse(equal, NA_character_, name)
      ) |>
        collapse::slt(title, download)
    )
  }) |>
    purrr::list_rbind() |>
    collapse::roworder(title)
}
