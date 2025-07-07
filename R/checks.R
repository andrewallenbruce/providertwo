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
  n_digits(x) == 10
}

#' @autoglobal
#' @noRd
are_10_digits <- function(x) {
  map_lgl(x, is_10_digits)
}

#' @autoglobal
#' @noRd
all_are_10_digits <- function(x) {
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

  orig <- x
  nine <- rm_last_digit(x)
  x    <- as.numeric(yank(strsplit(as.character(nine), "")))

  insitu::br_rev(x)
  insitu::br_mul(x, 2, idx = c(1, 3, 5, 7, 9))
  insitu::br_sub(x, 9, idx = which_(x > 9))
  x    <- sum(x)
  insitu::br_add(x, 24)

  chk  <- insitu::duplicate(x)

  insitu::br_div(chk, 10)
  insitu::br_ceil(chk)
  insitu::br_mul(chk, 10)
  insitu::br_sub(chk, y = x)

  identical(
    as.numeric(paste0(nine, chk)),
    orig
    )
}

# check_luhn(1234567890)
# check_luhn(c(1417918293, 1234567890))
# check_luhn(npi_all())
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
