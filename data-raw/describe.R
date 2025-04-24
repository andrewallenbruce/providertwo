x <- quick_pro("clinicians") |>
  mtt(
    address = cheapr_if_else(!is_na(adr_ln_2), paste(adr_ln_1, adr_ln_2), adr_ln_1),
    grd_yr = as.integer(grd_yr),
    num_org_mem = as.integer(num_org_mem),
    telehlth = val_match(telehlth, "N" ~ 0L, "Y" ~ 1L),
    ind_assgn = val_match(ind_assgn, "N" ~ 0L, "Y" ~ 1L),
    grp_assgn = val_match(grp_assgn, "N" ~ 0L, "Y" ~ 1L)) |>
  slt(
    -adr_ln_1,
    -adr_ln_2,
    -sec_spec_1,
    -sec_spec_2,
    -sec_spec_3,
    -sec_spec_4,
    -adrs_id,
    -ln_2_sprs
  ) |>
  rnm(pro_names$clinicians)

y <- clinicians(state = "GA")

x |>
  dplyr::glimpse()

z <- vctrs::vec_c(x, y) |> collapse::funique()

d <- collapse::descr(z)

d |> dplyr::glimpse()

desc_type <- function(x) {
  cheapr::new_df(
    column = colnames(x),
    type = purrr::map_vec(
      x,
      function(x)
        glue::glue("<{pillar::type_sum(x)}>")
    )
  )
}

f_IQR <- function(x) {
  diff(collapse::.quantile(as.numeric(x), c(0.25, 0.75)))
}

describe3 <- function(df, ...) {



  if (nargs() > 1) df <- dplyr::select(df, ...)

  dates <- dplyr::select(df, dplyr::where(function(x) inherits(x, "Date")))

  df <- dplyr::select(df, dplyr::where(function(x) !inherits(x, "Date")))

  sums <- dplyr::left_join(
    dplyr::reframe(
      dplyr::mutate(
        tidyr::pivot_longer(
          dplyr::mutate(
            df,
            dplyr::across(dplyr::where(is.character), stringr::str_length),
            dplyr::across(dplyr::where(function(x)
              is.factor(x) |
                is.logical(x)), as.numeric)
          ),
          dplyr::everything(),
          names_to = "column"
        ),
        n = 1 - cheapr::is_na(value)
      ),
      n = collapse::fsum(value, nthreads = 4L),
      min = collapse::fmin(value),
      mean = collapse::fmean(value, nthreads = 4L),
      iqr = fiqr(value),
      max = collapse::fmax(value),
      med = collapse::fmedian(value),
      sd = collapse::fsd(value),
      mad = mad(value, na.rm = TRUE),
      distribution = histo(value),
      .by = column
    ),
    get_type(df),
    by = dplyr::join_by(column)
  )
  topn <- function(x, limit = 10) {
    dplyr::tibble(
      column = names(x),
      uniq = collapse::fnunique(collapse::na_rm(x)),
      top = stringr::str_flatten_comma(dplyr::pull(
        dplyr::slice(
          dplyr::arrange(
            collapse::fcount(collapse::na_rm(x), name = "n"),
            dplyr::desc(n)
          ),
          seq(1, limit)
        ), x
      ))
    )
  }
  tops <- dplyr::filter(purrr::list_rbind(purrr::map(df, topn), names_to = "column"),
                        uniq != nrow(df))
  dplyr::select(
    dplyr::arrange(
      dplyr::left_join(sums, tops, by = dplyr::join_by(column)),
      dplyr::desc(type)
    ),
    column,
    type,
    n,
    min,
    mean,
    med,
    max,
    iqr,
    sd,
    mad,
    distribution,
    uniq,
    top
  )
}



