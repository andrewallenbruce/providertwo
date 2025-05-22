library(openxlsx2)

caid_chip_metrics_xlsx <- function(x) {
  x <- openxlsx2::read_xlsx(x, col_names = FALSE) |>
    as_tbl()

  col_names <- x[2, ] |>
    unlist(use.names = FALSE) |>
    greplace("\n", " ") |>
    set_names(LETTERS[1:ncol(x[2, ])])

  list(
    title = x$A[1],
    df    = x[3:nrow(x), ] |>
      mtt(across(B:J, as.integer),
          K = na_if(K, "blank")) |>
      pivot(values = 2:10) |>
      rnm(
        state = A,
        notes = K,
        metric = variable,
        count = value
      ) |>
      mtt(metric = unname(col_names[as.character(metric)])) |>
      colorder(state, metric, count)
  )
}

x <- the$catalogs$caid$resources$data |>
  _[[1]] |>
  _$download[1] |>
  openxlsx2::read_xlsx(col_names = FALSE) |>
  as_tbl()

x


caid_chip_renewal_xlsx <- function() {

}
