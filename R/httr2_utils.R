#' Parse Simple JSON Response
#' @param resp `<httr2_response>` object
#' @returns `<list>` Parsed JSON response
#' @autoglobal
#' @keywords internal
#' @export
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

#' Perform Iterative API request with Offset
#' @param req `<httr2_request>` object
#' @param limit `<int>` API rate limit, i.e. the maximum number of results an
#'                      API will return per request.
#' @returns list of `<httr2_response>`s
#' @keywords internal
#' @export
req_perform_iterative_offset <- function(req, limit) {

  req_perform_iterative(
    req,
    next_req        = iterate_with_offset(
      param_name    = "offset",
      start         = 0,
      offset        = limit,
      resp_complete = is_complete_with_limit(limit)
      )
    )
}
