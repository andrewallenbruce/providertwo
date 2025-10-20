#' @autoglobal
#' @noRd
match_query <- function(obj, qry) {

  pname <- rlang::names2(qry@params)
  clean <- field_switch(obj@fields)

  rlang::set_names(
    qry@params[qmatch(clean, pname)],
    obj@fields[sort(qmatch(pname, clean))]
  )
}

#' @autoglobal
#' @noRd
select_years_2G <- function(obj, qry) {

  x <- list(
    idx   = seq_along(obj@year),
    year  = obj@year,
    id    = obj@identifier,
    field = obj@fields@key)

  if (empty(qry@year)) {
    return(x)
  }

  x <- cheapr::list_modify(x,
    list(idx = obj@year %iin% qry@year))

  if (empty(x$idx)) {
    return(x)
  }

  cheapr::list_modify(x,
    list(
      year  = obj@year[x$idx],
      id    = obj@identifier[x$idx],
      field = obj@fields@key[x$idx]))
}

#' @autoglobal
#' @noRd
select_years_df <- function(obj, qry) {

  if (empty(qry@year)) {
    return(obj)
  }

  idx <- obj@year %iin% qry@year

  if (empty(idx)) {
    return(obj)
  }

  S7::props(obj) <- list(year = obj@year[idx], identifier = obj@identifier[idx])

  S7::props(obj@dimensions) <- list(total = obj@dimensions@total[idx])

  idy <- purrr::map(obj@fields@year, function(x) x %iin% qry@year)

  idy <- cheapr::which_(purrr::map_lgl(idy, Negate(rlang::is_empty)))

  if (identical(length(idy), length(obj@fields@year))) {
    return(obj)
  }

  S7::props(obj@fields) <- list(
    index = obj@fields@index[idy],
    year = obj@fields@year[idy])

  obj
}


#' @autoglobal
#' @noRd
match_query2 <- function(obj, qry) {

  if (empty(qry@params)) {
    return(select_years_df(obj, qry))
  }

  x <- select_years_df(obj, qry)

  # pname <- rlang::names2(qry@params)
  # clean <- field_switch(obj@fields@key)
  #
  # rlang::set_names(
  #   qry@params[qmatch(clean, pname)],
  #   obj@fields@key[sort(qmatch(pname, clean))]
  # )

  df <- cheapr::fast_df(raw = obj@fields@key) |>
    collapse::mtt(clean = field_switch(raw)) |>
    collapse::sbt(clean %iin% rlang::names2(qry@params)) |>
    collapse::mtt(qdx = unname(set_along(qry@params)[clean]))

  df <- purrr::map(
    x@year, function(yr)
      rlang::set_names(
        qry@params[df[df$year == yr, ]$qdx],
        df[df$year == yr, ]$field)) |>
    rlang::set_names(x@year)

  cheapr::list_modify(x, list(field = df))
}
