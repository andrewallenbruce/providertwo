#' @autoglobal
#' @noRd
match_query_G <- function(obj, qry) {

  pnames <- rlang::names2(qry@params)
  clean  <- clean_names(obj@fields@key)

  cheapr::list_combine(
    rlang::set_names(
      qry@params[qmatch(clean, pnames)],
      obj@fields@key[sort(qmatch(pnames, clean))]
    ),
    list(group = get_junctions(qry@groups))
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

  S7::props(obj) <- list(
    year       = obj@year[idx],
    identifier = obj@identifier[idx])

  S7::props(obj@dimensions) <- list(
    total = obj@dimensions@total[idx])

  idx_y <- cheapr::which_(obj@fields@set$year %in% qry@year)

  if (length(idx_y) == nrow(obj@fields@set)) {
    return(obj)
  }

  S7::props(obj@fields) <- list(set = cheapr::sset(obj@fields@set, idx_y))

  obj
}


#' @autoglobal
#' @noRd
match_query_2G <- function(obj, qry) {

  if (empty(qry@params)) {
    return(select_years_2G(obj, qry))
  }

  x <- select_years_2G(obj, qry)

  df <- purrr::imap(
    x$field, function(x, i)
      cheapr::new_df(year = i, field = x)) |>
    purrr::list_rbind() |>
    collapse::mtt(clean = clean_names(field)) |>
    collapse::sbt(clean %iin% rlang::names2(qry@params)) |>
    collapse::mtt(qdx = unname(set_along(qry@params)[clean]))

  df <- purrr::map(
    x$year, function(yr)
      rlang::set_names(
        qry@params[df[df$year == yr, ]$qdx],
        df[df$year == yr, ]$field)) |>
    rlang::set_names(x$year)

  cheapr::list_modify(x,
    list(
      field = df,
      group = get_junctions(qry@groups)))
}
