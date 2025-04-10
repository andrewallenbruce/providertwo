#' Search the NLM NPI Registry
#'
#' @param terms `<chr>` Search terms, separated by spaces
#' @returns `<tibble>` of search results
#' @examples
#' npi_nlm("john bethesda")
#' npi_nlm("Wiregrass Georgia")
#' npi_nlm("Dentist Valdosta")
#' npi_nlm("Valdosta")
#' @source [API Documentation: Individuals](https://clinicaltables.nlm.nih.gov/apidoc/npi_idv/v3/doc.html)
#' @source [API Documentation: Organizations](https://clinicaltables.nlm.nih.gov/apidoc/npi_org/v3/doc.html)
#' @autoglobal
#' @export
npi_nlm <- function(terms) {

  url <- "https://clinicaltables.nlm.nih.gov/api/npi_idv/v3/search?"

  parse_string <- \(x) resp_body_string(x) |> fparse(query = "/3") |> as_df()

  req <- request(url) |> req_url_query(terms = terms, maxList = 500L, count = 500L, .space = "form")

  n <- yank(perform_simple(req))

  if (n <= 500L) {
    cli_results(n, 500L, "NPPES", "NLM")
    return(req_perform(req) |> parse_string() |> set_names(c("full_name", "npi", "specialty", "full_address")) |> as_tbl())
  }

  if (n >= 7500L) cli_warn(c("!" = "{.strong {.val {n}}} Results Found", "v" = "Returning API limit of {.kbd 7500}."))

  req <- map(glue("{url}offset={offset_seq(n = if (n >= 7500L) 7499L else n, 500L)}"), \(x) request(x) |>
      req_url_query(terms = terms,
                    maxList = 500L,
                    count = 500L,
                    .space = "form"))

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
#'
#' @returns `<tibble>` of search results
#' @examples
#' #npi_nppes(npi = npi_ex$v)
#' @source [API Documentation](https://npiregistry.cms.hhs.gov/api-page)
#' @autoglobal
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


  x <- request("https://npiregistry.cms.hhs.gov/api/?version=2.1") |>
    req_url_query(
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
      limit                = 1200L,
      skip                 = 0L
    ) |>
    perform_simple()

  list(
    count   = x$result_count,
    results = x$results |> as_tbl()
  )
}
