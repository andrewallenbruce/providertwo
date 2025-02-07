#' Parse Simple JSON Response
#' @param resp `<httr2_response>` object
#' @returns `<list>` Parsed JSON response
#' @autoglobal
#' @noRd
resp_simple_json <- function(resp, ...) {
  resp_body_json(resp,
                 simplifyVector = TRUE,
                 check_type     = FALSE,
                 ...)
}

#' Helper for `iterate_with_offset`
#' @param limit `<int>` API rate limit, i.e. the maximum number of results an
#'                      API will return per request.
#' @returns `<function>` Function to check if API request is complete
#' @autoglobal
#' @keywords internal
#' @export
is_complete_with_limit <- function(limit) {

  # TODO: allow switching between different API limits?
  check_number_whole(limit, min = 1)

  function(resp) length(resp_body_json(resp)$data) < limit

}
