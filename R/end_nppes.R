#' @autoglobal
#' @noRd
parse_string <- \(x) resp_body_string(x) |> fparse(query = "/3") |> as_df()

#' @autoglobal
#' @noRd
nlm_url <- function(api) {

  api <- arg_match0(api, values = c("idv", "org"))

  glue("https://clinicaltables.nlm.nih.gov/api/",
       "npi_{api}",
       "/v3/search?")
}

#' Search the NLM NPI Registry
#'
#' @param terms `<chr>` Search terms, separated by spaces
#' @param npi `<chr>` Search terms, separated by spaces
#' @returns `<tibble>` of search results
#' @examples
#' npi_nlm("john bethesda")
#' npi_nlm("Wiregrass Georgia")
#' npi_nlm("Dentist Valdosta")
#' npi_nlm("Valdosta")
#' npi_nlm("Atlanta")
#' @source [API Documentation: Individuals](https://clinicaltables.nlm.nih.gov/apidoc/npi_idv/v3/doc.html)
#' @source [API Documentation: Organizations](https://clinicaltables.nlm.nih.gov/apidoc/npi_org/v3/doc.html)
#' @autoglobal
#' @rdname nppes
#' @export
npi_nlm <- function(terms, npi = NULL) {

  req <- nlm_url("idv") |>
    request() |>
    req_url_query(terms = terms, maxList = 500L, count = 500L, .space = "form")

    # req_url_query(ef = "NPI:npi,name.full:full_name,provider_type:specialty,addr_practice.full:full_address") |>
    # req_url_query(q = "NPI:1083618052")

  n <- yank(perform_simple(req))

  if (n <= 500L) {
    cli_results(n, 500L, "NPPES", "NLM")
    return(req_perform(req) |> parse_string() |> set_names(c("full_name", "npi", "specialty", "full_address")) |> as_tbl())
  }

  if (n >= 7500L) cli_warn(c("!" = "{.strong {.val {n}}} Results Found", "v" = "Returning API limit of {.kbd 7500}."))

  req <- map(glue('{nlm_url("idv")}offset={offset_seq(n = if (n >= 7500L) 7499L else n, 500L)}'), \(x) request(x) |>
               req_url_query(terms = terms, maxList = 500L, count = 500L, .space = "form"))

  cli_results(n = if (n > 7500L) 7500L else n, 500L, "NPPES", "NLM")

  resp <- req_perform_parallel(req, on_error = "continue")

  map(resp, parse_string) |>
    rowbind() |>
    set_names(c("full_name", "npi", "specialty", "full_address")) |>
    as_tbl()
}

#' Search the NPPES NPI Registry
#'
#' @param npi `<chr>` Search terms, separated by spaces
#' @param entity `<chr>` Entity type: `individual` or `organization`
#' @param first `<chr>` Individual provider's first name
#' @param last `<chr>` Individual provider's last name
#' @param organization `<chr>` Organizational provider's name
#' @param name_type `<chr>` Name purpose: `legal` or `de`
#' @param taxonomy_desc `<chr>` Taxonomy description
#' @param city `<chr>` City
#' @param state `<chr>` State
#' @param zip `<chr>` Zip code
#' @param country `<chr>` Country code
#' @returns `<tibble>` of search results
#' @examples
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

  if (length(args$number) > 1) return(.nppes_multi_npi(args$number))

    # address <- rsplit(yank(x[[1]]$addresses), ~ address_purpose)
    # address <- join(rnm(address$LOCATION, address_location = address_1), rnm(address$MAILING, address_mailing = address_1), overid = 2, verbose = 0)
    # taxonomies <- set_names(null_to_na(yank(x[[1]]$taxonomies)), c("taxonomy_code", "taxonomy_group", "taxonomy_desc", "taxonomy_state", "taxonomy_license", "taxonomy_primary"))
    # identifiers <- set_names(null_to_na(yank(x[[1]]$identifiers)), c("identifiers_code", "identifiers_desc", "identifiers_issuer", "identifiers_id", "identifiers_state"))
    #
    # res <- c(
    #   npi               = yank_index_name(x, number),
    #   entity            = convert_entity(yank_index_name(x, enumeration_type)),
    #   date_created      = convert_epoch(yank_index_name(x, created_epoch)),
    #   last_updated      = convert_epoch(yank_index_name(x, last_updated_epoch)),
    #   practiceLocations = null_to_na(yank(x[[1]]$practiceLocations)),
    #   other_names       = null_to_na(yank(x[[1]]$other_names)),
    #   endpoints         = null_to_na(yank(x[[1]]$endpoints)),
    #   inject(c(!!!x[[1]]$basic, !!!address, !!!taxonomies, !!!identifiers))
      # list(identifiers = identifiers)
      # ) |>
      # as_tbl()
      #
      # return(res)



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

  ("https://npiregistry.cms.hhs.gov/api/?version=2.1&" + glue("number={delist(npi_vec)}")) |>
    map(request) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    resps_data(\(resp) resp_body_string(resp) |> fparse() |> _[["results"]]) |>
    slt(-created_epoch, -last_updated_epoch) |>
    as_tbl()

}
