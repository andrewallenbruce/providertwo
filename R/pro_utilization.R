#' Utilization Class
#' @param npi `<chr>` 10-digit Individual NPI
#' @param pac `<chr>` 10-digit PECOS Associate Control (PAC) ID
#' @param first_name,middle_name,last_name,suffix `<chr>` Individual provider's name
#' @param procedure `<chr>` Procedure category
#' @param count `<int>` Number of procedures performed
#' @param percentile `<int>` Percentile of procedures performed
#' @returns An S7 `<proUtilization>` object.
#' @examplesIf interactive()
#' proUtilization(last_name = "CURRY", middle_name = "")
#' @autoglobal
#' @export
proUtilization <- new_class(
  name       = "proUtilization",
  package    = NULL,
  properties = list(
    args = new_property(
      class_list,
      getter = function(self)
        format_query_pro(self@args)),
    request = new_property(
      class_list,
      getter = function(self)
        base_request(self@request) |>
        req_url_query(!!!self@args)),
    count = new_property(
      class_integer,
      getter = function(self)
        req_url_query(
          self@request,
          count = "true",
          results = "false") |>
        perform_simple() |>
        _[["count"]]),
    response = new_property(
      new_union(NULL, class_list),
      getter = function(self) {

        if (self@count == 0L) return(NULL)

        perform_simple(self@request) |>
          _[["results"]]

      })),
  constructor = function(npi         = NULL,
                         pac         = NULL,
                         last_name   = NULL,
                         first_name  = NULL,
                         middle_name = NULL,
                         suffix      = NULL,
                         procedure   = NULL,
                         count       = NULL,
                         percentile  = NULL) {

    new_object(
      S7_object(),
      args = list2(
        "npi"                  = npi,
        "ind_pac_id"           = pac,
        "provider_last_name"   = last_name,
        "provider_first_name"  = first_name,
        "provider_middle_name" = middle_name,
        "suff"                 = suffix,
        "procedure_category"   = procedure,
        "count"                = count,
        "percentile"           = percentile),
      request = pro_endpoint("pdc_utilization")
    )
  }
)

#' Clinicians Utilization
#'
#' The Doctors and Clinicians utilization data file reports volume
#' information for procedures of interest on clinician profile pages and
#' in the provider data catalog (PDC) to inform patients and caregivers
#' about clinicians' experience.
#'
#' @param npi `<chr>` 10-digit Individual NPI
#' @param pac `<chr>` 10-digit PECOS Associate Control (PAC) ID
#' @param first_name,middle_name,last_name,suffix `<chr>` Individual provider's name
#' @param procedure `<chr>` Procedure category
#' @param count `<int>` Number of procedures performed
#' @param percentile `<int>` Percentile of procedures performed
#'
#' @returns `<tibble>` of search results
#'
#' @examplesIf interactive()
#' utilization(last_name = "CURRY")
#' utilization(npi = "1043245657")
#' utilization(npi = "1003001785")
#' @autoglobal
#' @export
utilization <- function(npi         = NULL,
                        pac         = NULL,
                        last_name   = NULL,
                        first_name  = NULL,
                        middle_name = NULL,
                        suffix      = NULL,
                        procedure   = NULL,
                        count       = NULL,
                        percentile  = NULL) {

  args <- list2(
    "npi"                  = npi,
    "ind_pac_id"           = pac,
    "provider_last_name"   = last_name,
    "provider_first_name"  = first_name,
    "provider_middle_name" = middle_name,
    "suff"                 = suffix,
    "procedure_category"   = procedure,
    "count"                = count,
    "percentile"           = percentile
  ) |>
    format_query_pro()

  req <- pro_endpoint("pdc_utilization") |>
    base_request() |>
    req_url_query(!!!args)

  n <- req_url_query(
    req,
    count = "true",
    results = "false") |>
    perform_simple() |>
    _[["count"]]

  if (!n) {
    return(NULL)
  }

  if (n) {
    req_url_query(
      req,
      count = "false",
      results = "true") |>
      perform_simple() |>
      _[["results"]] |>
      map_na_if() |>
      rnm(pro_names("utilization")) |>
      as_tbl()
  }
}
