#' Search the NPPES NPI Registry
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
#' @noRd
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
      as_fibble()
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
      resps_data(\(resp) parse_string(resp, query = "results")) |>
      slt(-created_epoch, -last_updated_epoch) |>
      as_fibble() |>
      rrapply(
        condition = \(x) !is.null(x),
        deflt     = NA_character_,
        how       = "list",
        options   = list(namesep  = "_",
                         simplify = TRUE))

  }
  resp
}

#' @autoglobal
#' @noRd
nlm_url <- function(api) {

  api <- arg_match0(api, values = c("idv", "org"))

  glue("https://clinicaltables.nlm.nih.gov/api/",
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
    request() |>
    req_url_query(
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
      req_perform(req) |>
        resp_body_string() |>
        fparse(query = "/3") |>
        as_df() |>
        set_names(c("full_name", "npi", "specialty", "full_address")) |>
        as_fibble()
    )
  }

  if (n >= 7500L)
    cli_warn(
      c("!" = "{.strong {.val {n}}} Results Found",
        "v" = "Returning API limit of {.kbd 7500}."))

  req <- map(
    glue('{nlm_url("idv")}offset={offset_seq(n = if (n >= 7500L) 7499L else n, 500L)}'),
    \(x)
    request(x) |>
      req_url_query(
        terms   = terms,
        maxList = 500L,
        count   = 500L,
        .space  = "form"
      )
  )

  cli_results(n = if (n > 7500L) 7500L else n, 500L, "NPPES", "NLM")

  resp <- req_perform_parallel(req, on_error = "continue")

  map(resp, \(x) x |>
        resp_body_string() |>
        fparse(query = "/3") |>
        as_df()) |>
    rowbind() |>
    set_names(c("full_name", "npi", "specialty", "full_address")) |>
    as_fibble()
}
