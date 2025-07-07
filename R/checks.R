#' @autoglobal
#' @noRd
last_digit <- function(x) {
  check_number_whole(x)
  x %% 10L
}

#' @autoglobal
#' @noRd
rm_last_digit <- function(x) {
  check_number_whole(x)
  x %/% 10L
}

#' @autoglobal
#' @noRd
n_digits <- function(x) {
  check_number_whole(x)
  ceiling(log10(x))
}

#' @autoglobal
#' @noRd
seq_n_digits <- function(x) {
  seq_len(n_digits(x))
}

#' @autoglobal
#' @noRd
sum_digits <- function(x) {
  sum(floor(x / 10L^(seq_n_digits(x) - 1L)) %% 10L)
}

#' @autoglobal
#' @noRd
is_10_digits <- function(x) {
  n_digits(x) != 10
}

#' @autoglobal
#' @noRd
are_10_digits <- function(x) {
  map_lgl(x, is_10_digits)
}

#' @autoglobal
#' @noRd
are_10_digits <- function(x) {
  map_lgl(x, is_10_digits)
}

#' @autoglobal
#' @noRd
all_10_digits <- function(x) {
  all(are_10_digits(x))
}

#' @autoglobal
#' @noRd
check_10_digits <- function(x, call = caller_env()) {
  if (n_digits(x) != 10) {
    cli::cli_abort(
      c("x" = "{.arg x} must have 10 digits"),
      call = call
    )
  }
}

# luhn(1417918293)
# luhn(1234567890)
# luhn(1234567893)
# luhn(123456789)
#' @autoglobal
#' @noRd
luhn <- function(x) {

  check_10_digits(x)

  original <- x
  nine     <- rm_last_digit(x)
  x        <- as.numeric(yank(strsplit(as.character(nine), "")))

  insitu::br_rev(x)
  insitu::br_mul(x, 2, idx = c(1L, 3L, 5L, 7L, 9L))
  insitu::br_sub(x, 9, idx = which_(x > 9L))
  x        <- sum(x)
  insitu::br_add(x, 24)

  checksum <- x / 10
  insitu::br_ceil(checksum)
  insitu::br_mul(checksum, 10)
  insitu::br_sub(checksum, y = x)

  identical(as.numeric(paste0(nine, checksum)), original)
}

# check_luhn(1234567890)
# check_luhn(c(1417918293, 1234567890))
# check_luhn(npi_ex$v)
#' @autoglobal
#' @noRd
check_luhn <- function(x) {
  if (length(x) > 1) {
    map_lgl(x, luhn)
  } else {
    luhn(x)
  }
}

#' @autoglobal
#' @noRd
assert_luhn <- function(x, call = caller_env()) {
  if (any(!check_luhn(x))) {
    invalid <- x[which_(check_luhn(x), invert = TRUE)]
    cli::cli_abort(
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
    cli::cli_abort(
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
    cli::cli_abort(
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
    cli::cli_abort(c("Invalid {.arg {xname}} entered: {.val {invalid}}"), call = call)
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
