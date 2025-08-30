#' @autoglobal
#' @noRd
param_names <- function(x) {
  rlang::names2(params(x))
}

#' @autoglobal
#' @noRd
remove_year <- function(x) {
  params(x)[param_names(x) %!=% "year"]
}

#' @autoglobal
#' @noRd
match_query <- function(obj, qry) {

  param  <- remove_year(qry)
  pname  <- rlang::names2(param)
  field  <- keys(obj)
  clean  <- clean_names(field)

  rlang::set_names(
    param[qmatch(clean, pname)],
    field[sort(qmatch(pname, clean))])
}

#' @autoglobal
#' @noRd
select_years <- function(obj, qry) {
  x <- list(
    idx   = seq_along(obj@year),
    year  = obj@year,
    id    = obj@identifier,
    field = keys(obj))

  if ("year" %!in_% param_names(qry)) {
    return(x)
  }

  idx <- cheapr::which_(obj@year %in_% params(qry)$year)

  if (rlang::is_empty(idx)) {
    return(x)
  }

  list(idx   = idx,
       year  = obj@year[idx],
       id    = obj@identifier[idx],
       field = keys(obj)[idx])
}

#' @autoglobal
#' @noRd
match_query2 <- function(obj, qry) {

  x <- select_years(obj, qry)

  if (identical("year", param_names(qry))) {
    return(x)
  }

  qdx   <- rlang::set_names(seq_along(qry@params), rlang::names2(qry@params))
  param <- remove_year(qry)

  df <- purrr::imap(x$field, function(x, i) {
    cheapr::new_df(year = i, field = x)
  }) |>
    purrr::list_rbind() |>
    collapse::mtt(clean = clean_names(field)) |>
    collapse::sbt(clean %in_% rlang::names2(param)) |>
    collapse::mtt(qdx = qdx[clean])

  x <- list(
    idx   = x$idx,
    year  = x$year,
    id    = x$id,
    field = df
  )

  x$field$qdx <- unname(x$field$qdx)

  fd <- purrr::map(
    collapse::funique(x$field$year), function(yr) {
      rlang::set_names(
        qry@params[x$field[x$field$year == yr, ]$qdx],
        x$field[x$field$year == yr, ]$field)
  }) |>
    rlang::set_names(collapse::funique(x$field$year))

  cheapr::list_modify(x, list(field = fd))
}
