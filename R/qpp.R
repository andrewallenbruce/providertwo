# quality_eligibility(npi = 1932365699, year = 2024)
# quality_eligibility(npi = 1043477615, year = 2018)
# quality_eligibility(npi = 1144544834, year = 2025)

#' Quality Payment Program
#' @name quality_payment
#' @param year A vector of years from 2018 to 2025
#' @param npi A vector of NPIs
#' @examples
#' quality_metrics(year = 2018:2025)
#'
#' quality_eligibility(year = 2018:2025,
#'                     npi  = c(1144544834, 1043477615, 1932365699))
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

# year = 2018:2025
# npi  = c(1144544834, 1043477615, 1932365699, npi_ex$k[1:5])
#' @autoglobal
#' @export
#' @rdname quality_payment
quality_eligibility <- function(year, npi) {

  check_required(year)
  check_required(npi)
  walk(year, \(year) check_number_whole(year, min = 2018, max = this_year()))

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
    list_rbind(names_to = "year") |>
    as_fibble()

  res <- res |>
    mtt(
      year                           = as.integer(year),
      nationalProviderIdentifierType = fmt_entity(nationalProviderIdentifierType),
      firstApprovedDate              = as_date(firstApprovedDate),
      specialty_description          = res$specialty$specialtyDescription,
      specialty_type                 = res$specialty$typeDescription,
      specialty_category             = res$specialty$categoryReference,
      specialty                      = NULL
    ) |>
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
    ) |>
    roworder(npi, -year)

  if (rlang::has_name(res, "error")) {
    res <- mtt(res, error = res$error$type) #|> rsplit(res$error)
  }
  res
}
