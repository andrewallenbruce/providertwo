#' Search the NPPES NPI Registry
#'
#' @param npi           `<chr>` Unique 10-digit National Provider Identifier number issued by CMS to US healthcare providers through NPPES.
#' @param entity        `<chr>` Entity type; one of either I for Individual (NPI-1) or O for Organizational (NPI-2)
#' @param first,last    `<chr>` Individual provider's name
#' @param organization  `<chr>` Organizational provider's name
#' @param name_type     `<chr>` Type of individual the first and last name parameters refer to; one of either AO for Authorized Officials or Provider for Individual Providers.
#' @param taxonomy_desc `<chr>` Provider's taxonomy description, e.g. Pharmacist, Pediatrics
#' @param city          `<chr>` City name. For military addresses, search for either APO or FPO.
#' @param state         `<chr>` 2-character state abbreviation. If it is the only input, one other parameter besides entype and country is required.
#' @param zip `<chr>` WC 5- to 9-digit zip code, without a hyphen.
#' @param country `<chr>` 2-character country abbreviation. Can be the only input, as long as it is not US.
#' @returns `<tibble>` of search results
#' @examplesIf interactive()
#' npi_nppes(npi = npi_ex$k[1:2]) |> str()
#' npi_nppes(npi = npi_ex$k[1]) |> str()
#' @source [API Documentation](https://npiregistry.cms.hhs.gov/api-page)
#' @autoglobal
#' @rdname nppes
#' @export
npi_nppes <- function(npi            = NULL,
                      entity         = NULL,
                      first          = NULL,
                      last           = NULL,
                      organization   = NULL,
                      name_type      = NULL,
                      taxonomy_desc  = NULL,
                      city           = NULL,
                      state          = NULL,
                      zip            = NULL,
                      country        = NULL) {

  args <- list2(
    number               = npi,
    enumeration_type     = entity,
    first_name           = first,
    last_name            = last,
    name_purpose         = name_type,
    organization_name    = organization,
    taxonomy_description = taxonomy_desc,
    city                 = city,
    state                = state,
    postal_code          = zip,
    country_code         = country,
    skip                 = 0L)

  if (length(args$number) > 1L) return(.nppes_multi_npi(args$number))

    args <- compact(args)

    "https://npiregistry.cms.hhs.gov/api/?version=2.1&limit=1200" |>
      request() |>
      req_url_query(!!!args) |>
      perform_simple() |>
      _[["results"]] |>
      slt(-created_epoch, -last_updated_epoch) |>
      as_tbl()
}

#' @autoglobal
#' @noRd
.nppes_multi_npi <- function(npi_vec) {

  resp <- glue("https://npiregistry.cms.hhs.gov/api/?version=2.1&",
               "number={delist(npi_vec)}") |>
    map(request) |>
    req_perform_parallel(on_error = "continue")

  if (length(resps_successes(resp)) > 0L) {

    resp <- resp |>
      resps_successes() |>
      resps_data(
        \(resp)
        resp_body_string(resp) |>
          fparse() |>
          _[["results"]]
        ) |>
      slt(-created_epoch,
          -last_updated_epoch) |>
      as_tbl() |>
      rrapply(
        condition = \(x) !is.null(x),
        deflt     = NA_character_,
        how       = "list",
        options   = list(namesep  = "_",
                         simplify = TRUE))

  }
  resp
}
