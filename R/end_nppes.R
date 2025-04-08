#' Search the NLM NPI Registry
#'
#' @param terms `<chr>` Search terms, separated by spaces
#'
#' @returns `<tibble>` of search results
#'
#' @examples
#' nppes_nlm("john bethesda")
#'
#' nppes_nlm("Dentist Valdosta")
#'
#' # Not returning all results
#' nppes_nlm("Valdosta")
#'
#' @source [API Documentation](https://clinicaltables.nlm.nih.gov/api/npi_idv/v3)
#'
#' @autoglobal
#' @export
nppes_nlm <- function(terms) {

  req <- request("https://clinicaltables.nlm.nih.gov/api/npi_idv/v3/search?") |>
    req_url_query(
      terms   = terms,
      maxList = 500L,
      count   = 500L,
      offset  = 0L,
      .space = "form"
    )

  resp <- perform_simple(req)
  n    <- yank(resp)
  # n <- resp[[1]]

  if (n > 7500L) {
    cli_abort(
      c("Your search returned {.strong {.val {n}}} results.",
        "x" = "The NLM API limit is {.strong {.emph 7,500}}.")
    )
  }

  results <- resp[[4]] |>
    as.data.frame() |>
    set_names(c("name", "npi", "specialty", "address")) |>
    as_tbl()

  nreq <- offset_size(n, 500L) > 1

  cli_results(n, 500L)

  if (false(nreq)) {

    return(results)

    } else {

      results2 <- req_perform_iterative(
      req,
      next_req        = iterate_with_offset(
        param_name    = "offset",
        start         = 500L,
        offset        = 500L,
        resp_complete = is_complete_with_limit(500L))) |>
      map(
        function(x) {
          x <- resp_simple_json(x)
          x[[4]] |>
            as.data.frame() |>
            set_names(c("name", "npi", "specialty", "address"))
        }) |>
      rowbind()

    as_tbl(rowbind(results, results2))
  }
}
