#' Luhn Algorithm Check for NPIs
#'
#' @param x `<chr>` NPI
#'
#' @returns `<lgl>` `TRUE` if valid NPI, `FALSE` otherwise
#'
#' @examples
#' luhn_check("1417918293")
#'
#' luhn_check("1234567890")
#'
#' luhn_check("1234567893")
#'
#' luhn_check("123456789")
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
luhn_check <- function(x) {

  idx <- c(1, 3, 5, 7, 9)

  id <- as_int(cheapr_rev(desplit(x)[1:9]))

  id[idx] <- id[idx] * 2

  id[idx] <- iif_else(id[idx] > 9, id[idx] - 9, id[idx])

  id <- sum(id) + 24

  ck <- (ceiling(id / 10) * 10) - id

  test <- sf_smush(sf_c(sf_sub(x, start = 1, stop = 9), as_chr(ck)))

  identical(test, x)
}

#' Luhn Algorithm Check for NPIs
#'
#' @param x `<chr>` NPI
#'
#' @returns `<lgl>` `TRUE` if valid NPI, `FALSE` otherwise
#'
#' @examples
#' check_luhn("1234567890")
#'
#' check_luhn(c("1417918293", "1234567890"))
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
check_luhn <- function(x) {

  if (length(x) > 1) {
    map_lgl(x, luhn_check)
  } else {
    luhn_check(x)
  }
}

#' Check Public API Limit
#' @param limit `<int>` API limit
#' @returns Nothing if valid, aborts otherwise
#' @autoglobal
#' @noRd
check_public_limit <- \(limit) {
  check_number_whole(limit, min = 0, max = 5000)
}

#' Check Provider API Limit
#' @param limit `<int>` API limit
#' @returns Nothing if valid, aborts otherwise
#' @autoglobal
#' @noRd
check_provider_limit <- \(limit) {
  check_number_whole(limit, min = 0, max = 2000)
}

#' Check Function parameters are valid API fields
#' @param params `<chr>` vector of function parameters
#' @param fields `<chr>` vector of API field names
#' @returns Nothing if valid, aborts otherwise
#' @autoglobal
#' @noRd
check_params_fields <- \(params, fields) {
  if (!any(params %in% fields)) {
    invalid_params <- setdiff(params, fields)
    abort(glue("Invalid Parameter(s): {invalid_params}"))
  }
}
