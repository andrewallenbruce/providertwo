#' Search the NPPES NPI Registry
#' @param npi `<chr>` Unique 10-digit National Provider Identifier number issued
#'    by CMS to US healthcare providers through NPPES.
#' @param entity_type `<int>` NPI entity/enumeration type; `1` for Individual
#'    Providers (NPI-1) or `2` for Organizational Providers (NPI-2). When
#'    specified, cannot be the only criteria entered.
#' @param first_name,last_name `<chr>` Individual provider's first/last names.
#'    Trailing wildcard entries require at least two characters to be entered
#'    (e.g., `"jo*"` ). Special characters allowed are: `&':,/-().$?;`.
#' @param org_name `<chr>` Organizational provider's name. Trailing wildcard
#'    entries require at least two characters to be entered. Special characters
#'    allowed are: `&'@:,/-().$?;`. All types of organization names (LBN, DBA,
#'    Former LBN, Other Name) associated with an NPI are examined for matches.
#'    As such, the results may contain a different name from the one entered.
#' @param name_type `<chr>` Refers to whether the first/last names entered
#'    pertains to an Authorized Official's name or a Provider's name. When `NULL`,
#'    will search against a provider's first and last name. `"AO"` will only
#'    search against an Authorized Official's name.
#' @param taxonomy `<chr>` Provider's taxonomy description, e.g. "Pharmacist", "Pediatrics"
#' @param city `<chr>` City associated with the provider's address identified
#'    in Address Purpose. To search for a Military Address enter either APO or
#'    FPO into the City field. This field allows the following special characters:
#'    ampersand, apostrophe, colon, comma, forward slash, hyphen, left and right
#'    parentheses, period, pound sign, quotation mark, and semi-colon.
#' @param state `<chr>` State abbreviation associated with the provider's
#'    address identified in Address Purpose. This field cannot be used as the
#'    only input criterion. If this field is used, at least one other field,
#'    besides `entity_type` and `country` is required. [Valid values](https://npiregistry.cms.hhs.gov/help-api/state) for states.
#' @param zip `<chr>` Postal Code associated with the provider's address
#'    identified in Address Purpose. If you enter a 5 digit postal code, it
#'    will match any appropriate 9 digit (zip+4) codes in the data. Trailing
#'    wildcard entries are permitted requiring at least two characters to be
#'    entered (e.g., `"21*"`).
#' @param country `<chr>` Country associated with the provider's address
#'    identified in Address Purpose. This field can be used as the only input
#'    criterion as long as the value selected is not US (United States).
#'    [Valid values](https://npiregistry.cms.hhs.gov/help-api/country) for countries.
#' @returns `<tibble>` of search results
#' @examplesIf interactive()
#' npi_nppes(npi = npi_ex$k[1:2]) |> str()
#' npi_nppes(npi = npi_ex$k[1]) |> str()
#' @source [NPPES API Help](https://npiregistry.cms.hhs.gov/api-page)
#' @source [NPPES NPI Registry Help](https://npiregistry.cms.hhs.gov/help/help-details)
#' @autoglobal
#' @rdname nppes
#' @noRd
npi_nppes <- function(npi = NULL,
                      entity = NULL,
                      first_name = NULL,
                      last_name = NULL,
                      org_name = NULL,
                      name_type = NULL,
                      taxonomy = NULL,
                      city = NULL,
                      state = NULL,
                      zip = NULL,
                      country = NULL) {

  args <- rlang::list2(
    number               = npi,
    enumeration_type     = entity,
    first_name           = first_name,
    use_first_name_alias = "True",
    last_name            = last_name,
    name_purpose         = name_type,
    organization_name    = org_name,
    # address_purpose      = address_purpose,
    taxonomy_description = taxonomy,
    city                 = city,
    state                = state,
    postal_code          = zip,
    country_code         = country,
    limit                = 1200L,
    skip                 = 0L)

  if (length(args$number) > 1L) return(.nppes_multi_npi(args$number))

    args <- purrr::compact(args)

    "https://npiregistry.cms.hhs.gov/api/?version=2.1&limit=1200" |>
      httr2::request() |>
      httr2::req_url_query(!!!args) |>
      perform_simple() |>
      _[["results"]] |>
      collapse::slt(-created_epoch, -last_updated_epoch) |>
      fastplyr::as_tbl()
}

#' @autoglobal
#' @noRd
.nppes_multi_npi <- function(npi_vec) {
  resp <- glue::glue(
    "https://npiregistry.cms.hhs.gov/api/?version=2.1&",
    "number={unlist(npi_vec, use.names = FALSE)}"
  ) |>
    purrr::map(request) |>
    httr2::req_perform_parallel(on_error = "continue")

  if (length(httr2::resps_successes(resp)) > 0L) {
    resp <- resp |>
      httr2::resps_successes() |>
      httr2::resps_data(\(resp) parse_string(resp, query = "results")) |>
      collapse::slt(-created_epoch, -last_updated_epoch) |>
      fastplyr::as_tbl() |>
      rrapply::rrapply(
        condition = \(x) ! is.null(x),
        deflt     = NA_character_,
        how       = "list",
        options   = list(namesep  = "_", simplify = TRUE)
      )

  }
  resp
}

#' @autoglobal
#' @noRd
nlm_url <- function(api) {
  api <- rlang::arg_match0(api, values = c("idv", "org"))

  glue::glue("https://clinicaltables.nlm.nih.gov/api/",
             "npi_{api}",
             "/v3/search?")
}

#' Search the NLM NPI Registry
#' @param terms `<chr>` Search terms, separated by spaces
#' @param npi `<chr>` Search terms, separated by spaces
#' @returns `<tibble>` of search results
#' @examplesIf interactive()
#' npi_nlm("john bethesda")
#' npi_nlm("Wiregrass Georgia")
#' npi_nlm("Dentist Valdosta")
#' npi_nlm("Valdosta")
#' npi_nlm("Atlanta")
#' @source [API Documentation: Individuals](https://clinicaltables.nlm.nih.gov/apidoc/npi_idv/v3/doc.html)
#' @source [API Documentation: Organizations](https://clinicaltables.nlm.nih.gov/apidoc/npi_org/v3/doc.html)
#' @autoglobal
#' @rdname nppes
#' @noRd
npi_nlm <- function(terms, npi = NULL) {
  req <- nlm_url("idv") |>
    httr2::request() |>
    httr2::req_url_query(
      terms   = terms,
      maxList = 500L,
      count   = 500L,
      .space  = "form"
    )

  # req_url_query(ef = "NPI:npi,name.full:full_name,
  # provider_type:specialty,addr_practice.full:full_address") |>
  # req_url_query(q = "NPI:1083618052")


  n <- perform_simple(req) |> _[[1]]

  if (n <= 500L) {
    cli_results(n, 500L, "NPPES", "NLM")
    return(
      httr2::req_perform(req) |>
        httr2::resp_body_string() |>
        RcppSimdJson::fparse(query = "/3") |>
        cheapr::as_df() |>
        rlang::set_names(c(
          "full_name", "npi", "specialty", "full_address"
        )) |>
        fastplyr::as_tbl()
    )
  }

  if (n >= 7500L)
    cli::cli_warn(c("!" = "{.strong {.val {n}}} Results Found", "v" = "Returning API limit of {.kbd 7500}."))

  req <- purrr::map(paste0(nlm_url("idv"), "offset=", offset(nres = if (n >= 7500L)
    7499L
    else
      n, 500L)), function(x)
        httr2::request(x) |>
      httr2::req_url_query(
        terms   = terms,
        maxList = 500L,
        count   = 500L,
        .space  = "form"
      ))

  cli_results(nres = if (n > 7500L) 7500L else n, 500L, "NPPES", "NLM")

  resp <- httr2::req_perform_parallel(req, on_error = "continue")

  purrr::map(resp, function(x)
    x |>
      httr2::resp_body_string() |>
      RcppSimdJson::fparse(query = "/3") |>
      cheapr::as_df()) |>
    collapse::rowbind() |>
    rlang::set_names(c("full_name", "npi", "specialty", "full_address")) |>
    fastplyr::as_tbl()
}
