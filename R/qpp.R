# quality_eligibility(npi = 1932365699, year = 2024)
# quality_eligibility(npi = 1043477615, year = 2018)
# quality_eligibility(npi = 1144544834, year = 2025)

#' Quality Payment Program
#' @name quality_payment
#' @param year A vector of years from 2018 to 2025
#' @param npi A vector of NPIs
#' @examples
#' quality_metrics(year = 2018:2025)
#' quality_eligibility(year = 2018, npi = c(1144544834, 1043477615, 1932365699))
#' quality_eligibility(year = 2024, npi = c(1144544834, 1043477615, 1932365699))
#' quality_eligibility(year = 2025, npi = c(1144544834, 1043477615, 1932365699))
NULL

#' @autoglobal
#' @export
#' @rdname quality_payment
quality_metrics <- function(year) {

  check_required(year)

  walk(year, \(year) check_number_whole(year, min = 2018, max = this_year()))

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

  check_required(year)
  check_required(npi)
  check_number_whole(year, min = 2018, max = this_year())

  x <- request("https://qpp.cms.gov/api/eligibility/npis/") |>
    req_url_path_append(paste(npi, collapse = ",")) |>
    req_url_query(year = year) |>
    req_headers(Accept = "application/vnd.qpp.cms.gov.v6+json") |>
    req_error(body = \(resp) resp_body_json(resp)$error$message) |>
    perform_simple() |>
    _[["data"]] |>
    as_fibble()

  x |>
    mtt(
      year                           = as.integer(year),
      nationalProviderIdentifierType = fmt_entity(nationalProviderIdentifierType),
      firstApprovedDate              = as_date(firstApprovedDate),
      specialty_description          = x$specialty$specialtyDescription,
      specialty_type                 = x$specialty$typeDescription,
      specialty_category             = x$specialty$categoryReference
    ) |>
    slt(-specialty) |>
    rnm(qpp_name("base")) |>
    colorder(
      year,
      npi,
      entity,
      last_name,
      first_name,
      middle_name,
      is_new,
      date_approved,
      enroll_year,
      years_in_medicare,
      specialty_category,
      specialty_type,
      specialty_description,
      is_maqi
    )
}
