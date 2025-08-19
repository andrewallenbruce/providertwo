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
  ceiling(log10(x))
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

matrix(data = 0L,
       nrow = 10L, # length(constants),
       ncol = 9L),
            dimnames = list(constants, axis))


matrix(as.numeric(1:6), 2, 3)

x <- npi_ex$k %/% 10L

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

first_million <- check_digit_vec(100000000:101000000)
second_million <- check_digit_vec(101000001:102000000)


hist(first_million)

collapse::fcount(first_million) |>
  collapse::roworder(x)
#    x      N
# 1  0 100000
# 2  1 100000
# 3  2 100001
# 4  3 100000
# 5  4 100000
# 6  5 100000
# 7  6 100000
# 8  7 100000
# 9  8 100000
# 10 9 100000

collapse::fcount(c(first_million, second_million)) |>
  collapse::roworder(x)
#    x      N
# 1  0 200001
# 2  1 200000
# 3  2 200000
# 4  3 200000
# 5  4 200000
# 6  5 200000
# 7  6 200000
# 8  7 200000
# 9  8 200000
# 10 9 200000

check_digit_dt <- function(x) {
  fastplyr::new_tbl(
    k = as.integer(x),
    check = check_digit_vec(x))
}

check_digit_dt(100000000:100100000) |>
  fastplyr::f_mutate(
    l = k %% 10L
  )

1_000_000_004
