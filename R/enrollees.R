#' Medicare Provider Enrollees
#'
#' Individual & Organizational Enrollment-level Data
#' on Providers Actively Approved to Bill Medicare.
#'
#' @section Accrual Periodicity:
#'    * `r roxy8601("R/P3M")`
#'
#' @section Links:
#'    * [Enrollment API](https://data.cms.gov/provider-characteristics/medicare-provider-supplier-enrollment/medicare-fee-for-service-public-provider-enrollment)
#'    * [Enrollment Data Dictionary](https://data.cms.gov/resources/medicare-fee-for-service-public-provider-enrollment-data-dictionary)
#'
#' @param npi `<chr>` 10-digit Individual NPI
#'
#' @param pac `<chr>` 10-digit PECOS Associate Control (PAC) ID
#'
#' @param enid `<chr>` 15-digit Medicare Enrollment ID
#'
#' @param spec_code `<chr>` Enrollment specialty code
#'
#' @param spec_desc `<chr>` Enrollment specialty description
#'
#' @param state `<chr>` Enrollment state abbreviation
#'
#' @param first,middle,last `<chr>` Individual provider's name
#'
#' @param org `<chr>` Organizational provider's name
#'
#'
#' @param gender `<chr>` Individual provider's gender:
#'
#'    * `"F"` (Female)
#'    * `"M"` (Male)
#'    * `"9"` (Unknown/Organization)
#'
#' @param limit `<int>` Maximum number of search results; default is `5000`
#'
#' @returns `<tibble>` of search results
#'
#' @examples
#' enrollees(enid = "I20040309000221")
#'
#' enrollees(npi = "1417918293", spec_code = "14-41")
#'
#' enrollees(pac = "2860305554", gender = "9")
#'
#' enrollees(state = "GA", gender = "9")
#'
#' enrollees(state = "GA", gender = "F")
#'
#' @autoglobal
#'
#' @export
enrollees <- function(npi       = NULL,
                      pac       = NULL,
                      enid      = NULL,
                      spec_code = NULL,
                      spec_desc = NULL,
                      first     = NULL,
                      middle    = NULL,
                      last      = NULL,
                      org       = NULL,
                      state     = NULL,
                      gender    = NULL,
                      limit     = 5000) {

  check_number_whole(limit)

  args <- list2(
    "NPI"                = npi,
    "PECOS_ASCT_CNTL_ID" = pac,
    "ENRLMT_ID"          = enid,
    "PROVIDER_TYPE_CD"   = spec_code,
    "PROVIDER_TYPE_DESC" = spec_desc,
    "STATE_CD"           = state,
    "FIRST_NAME"         = first,
    "MDL_NAME"           = middle,
    "LAST_NAME"          = last,
    "ORG_NAME"           = org,
    "GNDR_SW"            = gender)

  api <- public_Dataset("Public Provider Enrollment")

  nobs <- req_url_query(api@identifier@request, !!!format_query(args), size = limit) |>
    req_url_path_append("stats") |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE) |>
    gelm("found_rows") |>
    offset_sequence(limit = limit)

  is_complete <- \(resp) length(resp_body_json(resp)$data) < limit

  resp <- req_url_query(api@identifier@request, !!!format_query(args), size = limit) |>
    req_perform_iterative(
    next_req = iterate_with_offset(
      "offset",
      start = 0,
      offset = 5000,
      resp_complete = is_complete))


  cat(format(api@title), "\n")

  utils::formatUL(
    label  = "==>",
    offset = 2,
    c(paste0("Periodicity: ", format(api@accrualPeriodicity)),
      paste0("Last Modified:       ", format(api@modified)))
    ) |>
    writeLines()

  cat("\n")

  map(resp, \(x)
      resp_body_string(x) |>
        fparse() |>
        _[["data"]] |>
        qTBL()) |>
    rowbind() |>
    setNames(names(args)) |>
    vna_if()

  # qTBL(resp[["data"]]) |>
  #   setNames(names(args))

}
