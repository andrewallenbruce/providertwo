#' @autoglobal
#' @noRd
last_digit <- function(x) {
  check_number_whole(x)
  # x %% 10L
  insitu::br_mod(x, 10)
  x
}

#' @autoglobal
#' @noRd
rm_last_digit <- function(x) {
  check_number_whole(x)
  # x %/% 10L
  insitu::br_idiv(x, 10)
  x
}

#' @autoglobal
#' @noRd
n_digits <- function(x) {
  check_number_whole(x)
  # ceiling(log10(x))
  insitu::br_log10(x)
  insitu::br_ceil(x)
  x
}

#' @autoglobal
#' @noRd
seq_n_digits <- function(x) {
  seq_len(ceiling(log10(x)))
}

#' @autoglobal
#' @noRd
sum_digits <- function(x) {
  sum(floor(x / 10L^(seq_n_digits(x) - 1L)) %% 10L)
}

#' @autoglobal
#' @noRd
is_10_digits <- function(x) {
  # n_digits(x) == 10
  insitu::br_log10(x)
  insitu::br_ceil(x)
  insitu::br_eq(x, 10)
  x
}

#' @autoglobal
#' @noRd
all_are_10_digits <- function(x) {
  all(is_10_digits(x))
}

#' @autoglobal
#' @noRd
check_10_digits <- function(x, call = caller_env()) {
  if (n_digits(x) != 10) {
    cli::cli_abort(c("x" = "{.arg x} must have 10 digits"), call = call)
  }
}

#' @autoglobal
#' @noRd
explode_number <- function(x) {
  as.integer(x) |>
  as.character() |>
    strsplit("") |>
    yank() |>
    as.numeric()
}

# x <- npi_all()
# insitu::br_idiv(x, 10)
# length(x)
# bench::mark(
#   base = purrr::map_int(x, check_base),
#   insitu = map_int(x, check_insitu)
  # iterations = 10,
  # check = FALSE
# )

# bench::mark(
#   base = check_base(100000000:100000003),
#   insitu = check_insitu(100000000:100000003)
  # iterations = 10,
  # check = FALSE
# )

# check_base(100000000)
# check_base(100000001)
# check_base(100000002)
# check_base(100000003)
#' @autoglobal
#' @noRd
check_base <- function(x) {
  x <- rev(as.integer(unlist(strsplit(as.character(as.integer(x)), ""))))
  x[c(1L, 3L, 5L, 7L, 9L)]  <- x[c(1L, 3L, 5L, 7L, 9L)] * 2L
  x[cheapr::which_(x > 9L)] <- x[cheapr::which_(x > 9L)] - 9L
  x <- z <- sum(x) + 24
  ceiling(x / 10L) * 10L - z
}

# check_insitu(100000000)
# check_insitu(100000001)
# check_insitu(100000002)
# check_insitu(100000003)
#' @autoglobal
#' @noRd
check_insitu <- function(x) {
  x <- as.numeric(unlist(strsplit(as.character(as.integer(x)), "")))
  insitu::br_rev(x)
  insitu::br_mul(x, 2, idx = c(1, 3, 5, 7, 9))
  insitu::br_sub(x, 9, idx = cheapr::which_(x > 9))
  x <- sum(x)
  insitu::br_add(x, 24)
  z <- insitu::duplicate(x)
  insitu::br_div(x, 10)
  insitu::br_ceil(x)
  insitu::br_mul(x, 10)
  insitu::br_sub(x, z)
  x
}

#' @autoglobal
#' @noRd
check_digit_vec <- function(x) {

  if (all(ceiling(log10(x)) != 9L)) {
    stop("All numbers must have 9 digits")
  }

  i <- c(1L, 3L, 5L, 7L, 9L)
  x <- strsplit(as.character(as.integer(x)), "")

  x <- purrr::map(x, \(x) {
    x <- rev(as.integer(x))
    x[i]  <- x[i] * 2L
    x[cheapr::which_(x > 9L)] <- x[cheapr::which_(x > 9L)] - 9L
    x
  })

  x <- z <- collapse::fsum(x) + 24

  as.integer(ceiling(x / 10L) * 10L - z)
}


# luhn_base(1417918293)
# luhn_base(1234567890)
# luhn_base(1234567893)
# luhn_base(123456789)
#' @autoglobal
#' @noRd
luhn_base <- function(x) {
  check <- x %% 10L
  x <- rev(as.integer(unlist(strsplit(as.character(as.integer(x %/% 10L)), ""))))
  x[c(1L, 3L, 5L, 7L, 9L)]  <- x[c(1L, 3L, 5L, 7L, 9L)] * 2L
  x[cheapr::which_(x > 9L)] <- x[cheapr::which_(x > 9L)] - 9L
  x <- z <- sum(x) + 24
  x <- ceiling(x / 10L) * 10L - z
  x == check
}

# luhn_insitu(1417918293)
# luhn_insitu(1234567890)
# luhn_insitu(1234567893)
# luhn_insitu(123456789)
#' @autoglobal
#' @noRd
luhn_insitu <- function(x) {
  checksum <- x %% 10L
  insitu::br_idiv(x, 10)
  x <- explode_number(x)
  insitu::br_rev(x)
  insitu::br_mul(x, 2, idx = c(1, 3, 5, 7, 9))
  insitu::br_sub(x, 9, idx = cheapr::which_(x > 9))
  x <- sum(x)
  insitu::br_add(x, 24)
  z <- insitu::duplicate(x)
  insitu::br_div(x, 10)
  insitu::br_ceil(x)
  insitu::br_mul(x, 10)
  insitu::br_sub(x, z)
  insitu::br_eq(x, checksum)
  x
  # x == checksum
}

# x        <- npi_all()
# x[[100]] <- 1003011300
# x[[951]] <- 1003111300
# x[[673]] <- 1103111300
# bench::mark(
  # int_luhn = map_int(npi_, luhn_algo),
#   lgl_luhn = map_int(x, luhn_algo),
#   iterations = 10,
#   check = FALSE
# )
# purrr::map_vec(npi_, luhn_algo)
# luhn_check(1234567890)
# luhn_check(c(1417918293, 1234567890))
# luhn_check(npi_all())
#' @autoglobal
#' @noRd
luhn_check <- function(x) {
  if (length(x) > 1L) {
    purrr::map_int(x, luhn_algo) == 1L
  } else {
    luhn_algo(x) == 1L
  }
}

# x        <- npi_all()
# x[[100]] <- 1003011300
# x[[951]] <- 1003111300
# x[[673]] <- 1103111300
# assert_luhn(x)
#' @autoglobal
#' @noRd
assert_luhn <- function(x, call = caller_env()) {
  if (any(!luhn_check(x))) {
    idx <- cheapr::which_(purrr::map_int(x, luhn_algo) == 0L)
    cli::cli_abort(
      c(
        "x" = "{.val {length(idx)}} {.field npi{?s}} failed Luhn algorithm check:",
        cli::col_yellow(
          cli::format_bullets_raw(
            paste(cli::symbol$arrow_right, just_right(brackets(idx)), x[idx])))),
      call = call
    )
  }
}

# @autoglobal
# @noRd
# assert_digits <- function(x, call = caller_env()) {
#   if (any(!grepl("^[0-9]{10}$", x = as.character(x), perl = TRUE))) {
#     invalid <- x[which_(grepl('^[0-9]{10}$', x = as.character(x), perl = TRUE), invert = TRUE)]
#     cli::cli_abort(
#       c("Invalid {.arg npi} entered: {.val {invalid}}", "x" = "{.arg npi} must be all digits"),
#       call = call
#     )
#   }
# }

# @autoglobal
# @noRd
# assert_nchars <- function(x, n, xname, call = caller_env()) {
#   if (any(nchar(as.character(x)) != n)) {
#     invalid <- x[which_(nchar(as.character(x)) != n)]
#     cli::cli_abort(
#       c("Invalid {.arg {xname}} entered: {.val {invalid}}", "x" = "{.arg {xname}} must be {n} characters long"),
#       call = call
#     )
#   }
# }

# @autoglobal
# @noRd
# assert_choices <- function(x, choices, xname, call = caller_env()) {
#   if (any(!x %in% choices)) {
#     invalid <- x[which_(x %in% choices, invert = TRUE)]
#     cli::cli_abort(c("Invalid {.arg {xname}} entered: {.val {invalid}}"), call = call)
#   }
# }

# check_params_fields <- function(params, fields) {
#   if (!any(params %in% fields)) {
#     invalid_params <- setdiff(params, fields)
#     abort(glue("Invalid Parameter(s): {invalid_params}"))
#   }
# }
