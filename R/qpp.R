#' Quality Payment Program
#' @name quality_payment
#' @param year A vector of years from 2018 to 2025
#' @param npi A vector of NPIs
#' @examples
#' quality_metrics(year = 2018:2025)
#'
#' quality_eligibility(
#'    year = 2018:2024,
#'    npi = c(1144544834, 1043477615, 1932365699, 1225701881))
#'
#' build(
#'    endpoint("qppe"),
#'    query(npi = any_of(1144544834, 1043477615, 1932365699, 1225701881)))
NULL

#' @autoglobal
#' @export
#' @rdname quality_payment
quality_metrics <- function(year) {

  check_required(year)
  check_years(year, min = 2018)

  map(year, \(y) {
    fibble(
      year     = rep.int(as.integer(y), 4L),
      category = c(rep("Individual", 2L), rep("Group", 2L)) |> factor_(),
      metric   = rep(c("HCC Risk Score", "Dual Eligibility Ratio"), 2L) |> factor_(),
      mean     = request("https://qpp.cms.gov/api/eligibility/stats") |>
        req_url_query(year = y) |>
        perform_simple() |>
        get_elem("data") |>
        unlist(use.names = FALSE))
    }
  ) |>
    rowbind() |>
    roworder(metric, category)
}

#' @autoglobal
#' @export
#' @rdname quality_payment
quality_eligibility <- function(year, npi) {

  check_requires(list(year, npi))
  check_years(year, min = 2018)

  url <- as_glue("https://qpp.cms.gov/api/eligibility/npis/") +
    glue_collapse(npi, sep = ",") +
    glue("?year={year}")

  res <- map(url, function(x)
    request(x) |>
      req_headers(Accept = "application/vnd.qpp.cms.gov.v6+json") |>
      req_error(body = \(resp) resp_body_json(resp)$error$message) |>
      perform_simple() |>
      get_elem("data")) |>
    set_names(year) |>
    list_rbind(names_to = "year")

  if (rlang::has_name(res, "error")) res$error <- NULL

  res |>
    as_fibble() |>
    mtt(
      year                           = as.integer(year),
      nationalProviderIdentifierType = fmt_entity(nationalProviderIdentifierType),
      firstApprovedDate              = as_date(firstApprovedDate),
      specialty                      = glue::glue("{res$specialty$specialtyDescription} ({res$specialty$typeDescription}) [{res$specialty$categoryReference}]")
    ) |>
    rnm(firstName                      = "first_name",
        middleName                     = "middle_name",
        lastName                       = "last_name",
        nationalProviderIdentifierType = "entity",
        newlyEnrolled                  = "is_new",
        firstApprovedDate              = "date_enrolled",
        isMaqi                         = "is_maqi",
        # qpStatus                       = "qp_status",
        # qpScoreType                    = "qp_score_type",
        # amsMipsEligibleClinician       = "ams_mips_elig",
        organizations                  = "ORGS") |>
    colorder(
      year,
      npi,
      entity,
      last_name,
      first_name,
      middle_name,
      is_new,
      date_enrolled,
      is_maqi
    ) |>
    roworder(npi, -year)
}
