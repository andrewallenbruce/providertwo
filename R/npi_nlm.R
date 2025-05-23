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
#' @export
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
        as_tbl()
      )
  }

  if (n >= 7500L)
    cli::cli_warn(c("!" = "{.strong {.val {n}}} Results Found",
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
    as_tbl()
}
