#' @autoglobal
#' @noRd
resp_simple_json <- function(resp, ...) {
  resp_body_json(resp, simplifyVector = TRUE, check_type = FALSE, ...)
}

#' @autoglobal
#' @noRd
perform_simple <- function(req, ...) {
  req_perform(req) |>
    # resp_check_status(resp, info = args) |>
    resp_simple_json(...)
}

#' @autoglobal
#' @noRd
perform_simple_request <- function(x, ...) {
  x |>
    request() |>
    req_perform() |>
    resp_simple_json(...)
}

#' @autoglobal
#' @noRd
perform_parallel <- function(x) {
  resp_list <- map(x, request) |>
    req_perform_parallel(on_error = "continue")

  parse_string <- \(x) fparse(resp_body_string(x), query = "/data")

  resps_successes(resp_list) |>
    map(parse_string) |>
    rowbind() |>
    map_na_if() |>
    as_tbl()
}
