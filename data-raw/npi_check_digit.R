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
