#' Quality Payment Program
#' @name quality_payment
#' @param year A vector of years from 2018 to 2025
#' @param npi A vector of NPIs
#' @examples
#' qpp_stats(year = 2018:2025) |> print(n = Inf)
#' qpp_elig(npi = 1043477615, year = 2018)
#' qpp_elig(npi = 1144544834, year = 2025)
#' qpp_elig(npi = 1932365699, year = 2024)
#' qpp_elig(year = 2018, npi = c(1144544834, 1043477615, 1932365699))
#' qpp_elig(year = 2024, npi = c(1144544834, 1043477615, 1932365699))
NULL

#' @autoglobal
#' @export
#' @rdname quality_payment
qpp_stats <- function(year) {

  walk(year, \(year) check_number_whole(year, min = 2018, max = 2025))

  map(year, \(y) {
    new_tbl(
      year     = rep(as.integer(y), 4),
      category = c(rep("Individual", 2), rep("Group", 2)) |> factor_(),
      metric   = rep(c("HCC Risk Score", "Dual Eligibility Ratio"), 2) |> factor_(),
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
qpp_elig <- function(year, npi) {

  check_number_whole(year, min = 2018, max = 2025)

  x <- request("https://qpp.cms.gov/api/eligibility/npis/") |>
    req_url_path_append(paste(npi, collapse = ",")) |>
    req_url_query(year = year) |>
    req_headers(Accept = "application/vnd.qpp.cms.gov.v6+json") |>
    req_error(body = \(resp) resp_body_json(resp)$error$message) |>
    perform_simple() |>
    _[["data"]] |>
    as_tbl()

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

#' @autoglobal
#' @noRd
qpp_name <- function(x, call = caller_env()) {
  switch(
    x,
    base = c(
      firstName                      = "first_name",
      middleName                     = "middle_name",
      lastName                       = "last_name",
      nationalProviderIdentifierType = "entity",
      newlyEnrolled                  = "is_new",
      firstApprovedDate              = "date_approved",
      pecosEnrollmentDate            = "enroll_year",
      yearsInMedicare                = "years_in_medicare",
      isMaqi                         = "is_maqi",
      organizations                  = "ORGS"
    ),
    organizations = c(
      ind_pac_id                                 = "pac",
      ind_enrl_id                                = "enid",
      provider_last_name                         = "last_name",
      provider_first_name                        = "first_name",
      provider_middle_name                       = "middle_name",
      suff                                       = "suffix_name",
      cred                                       = "credential",
      gndr                                       = "gender",
      grd_yr                                     = "grad_year",
      med_sch                                    = "med_school",
      pri_spec                                   = "spec_prim",
      sec_spec_all                               = "spec_sec",
      telehlth                                   = "telehealth",
      org_pac_id                                 = "org_pac",
      num_org_mem                                = "org_members",
      citytown                                   = "city",
      state                                      = "state",
      zip_code                                   = "zip",
      telephone_number                           = "phone"
    ),
    individualScenario = c(
      ind_pac_id                                 = "pac",
      provider_last_name                         = "last_name",
      provider_first_name                        = "first_name",
      provider_middle_name                       = "middle_name",
      profile_display_indicator                  = "display"
    ),
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
}

# x <- request("https://qpp.cms.gov/api/eligibility/stats") |>
# req_headers(Accept = "application/vnd.qpp.cms.gov.v6+json") |>
# req_url_query(year = year) |> perform_simple() |>
# get_elem("data") |> unlist(use.names = FALSE)
