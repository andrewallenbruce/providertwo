test <- 1000000000:1000100000 |>
  as.character() |>
  strsplit(split = "") |>
  map(as.integer) |>
  unlist() |>
  matrix(ncol = 10, byrow = TRUE) |>
  as.data.frame()

step2 <- \(x) ifelse(x > 9L, x - 9L, x)

test |>
  rnm(V10 = ORIG) |>
  mtt(
    V2 = V2 * 2,
    V4 = V4 * 2,
    V6 = V6 * 2,
    V8 = V8 * 2,
    V2 = step2(V2),
    V4 = step2(V4),
    V6 = step2(V6),
    V8 = step2(V8),
    RSUM = V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + 24,
    CHK = (ceiling(RSUM / 10) * 10) - RSUM,
    VALID = ORIG == CHK
  ) |>
  # sbt(VALID == TRUE) |> tail()
  fcount(VALID)
