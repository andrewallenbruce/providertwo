#' @include query.R

#' @autoglobal
#' @noRd
params <- S7::new_generic("params", "obj", function(obj) {
  S7::S7_dispatch()
})

S7::method(params, class_query) <- function(obj) {
  S7::prop(obj, "params")
}

#' @autoglobal
#' @noRd
key <- S7::new_generic("key", "obj", function(obj) {
  S7::S7_dispatch()
})

S7::method(key, class_group) <- function(obj) {
  S7::prop(obj, "members") |> purrr::map(key)
}

S7::method(key, class_catalog) <- function(obj) {
  S7::prop(obj, "access") |> key()
}

S7::method(key, class_endpoint) <- function(obj) {
  S7::prop(obj, "fields") |> key()
}

S7::method(key, class_fields) <- function(obj) {
  S7::prop(obj, "key")
}

#' @autoglobal
#' @noRd
param_names <- function(x) {
  rlang::names2(params(x))
}

#' @autoglobal
#' @noRd
not_year <- function(x) {
  params(x)[param_names(x) %!=% "year"]
}

#' @autoglobal
#' @noRd
match_query <- function(obj, qry) {

  param   <- not_year(qry)
  p_name  <- rlang::names2(param)
  field   <- key(obj)
  clean   <- clean_names(field)

  rlang::set_names(
    param[qmatch(clean, p_name)],
    field[sort(qmatch(p_name, clean))])
}

#' @autoglobal
#' @noRd
select_years <- function(obj, qry) {
  x <- list(
    idx   = seq_along(obj@year),
    year  = obj@year,
    id    = obj@identifier,
    field = key(obj))

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
       field = key(obj)[idx])
}

#' @autoglobal
#' @noRd
match_query2 <- function(obj, qry) {

  x <- select_years(obj, qry)

  if (identical("year", param_names(qry))) {
    return(x)
  }

  qdx   <- rlang::set_names(seq_along(qry@params), rlang::names2(qry@params))
  param <- not_year(qry)

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

#' @autoglobal
#' @noRd
match_dict <- function(obj, qry) {

  param   <- not_year(qry)
  p_name  <- rlang::names2(param)
  field   <- key(obj)
  clean   <- clean_names(field)

  # the$dict |>
  #   collapse::sbt(alias %==% obj@metadata$alias) |>
  #   _$data |>
  #   yank() |>
  #   collapse::sbt(type %iin% p_name)

  alias_match <- yank(
    the$dict$data[
      the$dict$alias %==% obj@metadata$alias
      ])$type

  idx_param <- p_name %iin% alias_match

  names(param[idx_param]) <- p_name[idx_param]

  param
}

#' @autoglobal
#' @noRd
match_dict2 <- function(obj, qry) {

  x <- select_years(obj, qry)

  if (identical("year", param_names(qry))) {
    return(x)
  }

  qdx   <- rlang::set_names(seq_along(qry@params), rlang::names2(qry@params))
  param <- not_year(qry)

  the$dict |>
    collapse::sbt(alias %==% obj@metadata$alias) |>
    _$data |>
    yank() |>
    collapse::sbt(year %iin% x$year) |>
    collapse::sbt(type %iin% names(param))


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
