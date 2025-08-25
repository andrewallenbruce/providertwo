#' Quality Payment Program
#' @name quality_payment
#' @param year A vector of years from 2018 to 2025
#' @param npi A vector of NPIs
#' @examples
#' qpp_metrics <- quality_metrics(year = 2018:2025)
#'
#' qpp_eligible <- quality_eligibility(
#'   year = 2018:2024,
#'   npi = c(1144544834, 1043477615, 1932365699, 1225701881))
#'
#' qpp_experience <- build(
#'   endpoint("qppe"),
#'   query(npi = any_of(1144544834, 1043477615, 1932365699, 1225701881)))
#'
#'
#' qpp_exp <- qpp_experience@string |>
#'   providertwo:::gremove("/stats") |>
#'   providertwo:::greplace("size=1", "size=5000") |>
#'   providertwo:::map_perform_parallel() |>
#'   providertwo:::set_clean(qpp_experience@year) |>
#'   purrr::list_rbind(names_to = "prog_year") |>
#'   providertwo:::map_na_if() |>
#'   fastplyr::as_tbl()
#'
#' list(
#'   experience  = providertwo:::set_clean(qpp_exp, names(qpp_exp)),
#'   eligibility = qpp_eligible,
#'   metrics     = qpp_metrics)
NULL

#' @autoglobal
#' @export
#' @rdname quality_payment
quality_metrics <- function(year) {

  check_required(year)
  check_years(year, min = 2018)

  purrr::map(year, function(y) {
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

  url <- glue::as_glue("https://qpp.cms.gov/api/eligibility/npis/") +
    glue::glue_collapse(npi, sep = ",") +
    glue::glue("?year={year}")

  res <- purrr::map(url, function(x)
    httr2::request(x) |>
      httr2::req_headers(Accept = "application/vnd.qpp.cms.gov.v6+json") |>
      httr2::req_error(
        body = function(resp)
          httr2::resp_body_json(resp)$error$message
      ) |>
      httr2::req_perform() |>
      httr2::resp_body_json(simplifyVector = TRUE, check_type = FALSE) |>
      collapse::get_elem("data")) |>
    rlang::set_names(year) |>
    purrr::list_rbind(names_to = "prog_year")

  if (rlang::has_name(res, "error")) res$error <- NULL

  res |>
    collapse::mtt(
      prog_year                      = as.integer(prog_year),
      nationalProviderIdentifierType = fmt_entity(nationalProviderIdentifierType),
      firstApprovedDate              = as_date(firstApprovedDate),
      specialty                      = glue::glue("{res$specialty$specialtyDescription} [D] {res$specialty$typeDescription} [T] {res$specialty$categoryReference} [C]") |>
        as.character()
    ) |>
    collapse::rnm(
      firstName                      = "first_name",
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
    collapse::colorder(
      prog_year,
      npi,
      entity,
      last_name,
      first_name,
      middle_name,
      specialty,
      date_enrolled,
      is_new,
      is_maqi
    ) |>
    collapse::roworder(npi, -prog_year) |>
    fastplyr::as_tbl()
}
