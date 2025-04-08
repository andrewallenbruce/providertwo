# luhn_check("1417918293")
# luhn_check("1234567890")
# luhn_check("1234567893")
# luhn_check("123456789")
#' @autoglobal
#' @noRd
luhn_check <- function(x) {

  i <- c(1L, 3L, 5L, 7L, 9L)
  d <- cheapr_rev(as.integer(strsplit(delist(as.character(x)), "")[[1]][1:9]))

  d[i] <- d[i] * 2L
  d[i] <- ifelse(d[i] > 9L, d[i] - 9L, d[i])

  d <- sum(d) + 24L
  d <- (ceiling(d / 10) * 10) - d

  identical(paste0(substr(x, 1, 9), d), x)
}

# check_luhn("1234567890")
# check_luhn(c("1417918293", "1234567890"))
# check_luhn(npi_ex)
#' @autoglobal
#' @noRd
check_luhn <- function(x) {
  if (length(x) > 1) {
    map_lgl(x, luhn_check)
  } else {
    luhn_check(x)
  }
}

#' @autoglobal
#' @noRd
assert_luhn <- function(x, call = caller_env()) {
  if (any(!check_luhn(x))) {
    invalid <- x[which_(check_luhn(x), invert = TRUE)]
    cli_abort(
      c("Invalid {.arg npi} entered: {.val {invalid}}", "x" = "{.arg npi} must pass Luhn algorithm"),
      call = call
    )
  }
}

#' @autoglobal
#' @noRd
assert_digits <- function(x, call = caller_env()) {
  if (any(!grepl("^[0-9]{10}$", x = as.character(x), perl = TRUE))) {
    invalid <- x[which_(grepl('^[0-9]{10}$', x = as.character(x), perl = TRUE), invert = TRUE)]
    cli_abort(
      c("Invalid {.arg npi} entered: {.val {invalid}}", "x" = "{.arg npi} must be all digits"),
      call = call
    )
  }
}

#' @autoglobal
#' @noRd
assert_nchars <- function(x, n, xname, call = caller_env()) {
  if (any(nchar(as.character(x)) != n)) {
    invalid <- x[which_(nchar(as.character(x)) != n)]
    cli_abort(
      c("Invalid {.arg {xname}} entered: {.val {invalid}}", "x" = "{.arg {xname}} must be {n} characters long"),
      call = call
    )
  }
}

#' @autoglobal
#' @noRd
assert_choices <- function(x, choices, xname, call = caller_env()) {
  if (any(!x %in% choices)) {
    invalid <- x[which_(x %in% choices, invert = TRUE)]
    cli_abort(c("Invalid {.arg {xname}} entered: {.val {invalid}}"), call = call)
  }
}

# check_params_fields <- function(params, fields) {
#   if (!any(params %in% fields)) {
#     invalid_params <- setdiff(params, fields)
#
#     abort(glue("Invalid Parameter(s): {invalid_params}"))
#
#   }
# }
