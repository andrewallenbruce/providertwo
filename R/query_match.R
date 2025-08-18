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
input <- S7::new_generic("input", "obj", function(obj) {
  S7::S7_dispatch()
})

S7::method(input, class_query) <- function(obj) {
  S7::prop(obj, "input")
}

#' @autoglobal
#' @noRd
remove_year <- function(x) {
  params(x)[names2(params(x)) %!=% "year"]
}

#' @autoglobal
#' @noRd
match_query <- function(obj, qry) {

  param  <- remove_year(qry)
  pname  <- names2(param)
  field  <- keys(obj)
  clean  <- clean_names(field)

  set_names(
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

  if ("year" %!in_% names2(params(qry))) {
    return(x)
  }

  idx <- which_(obj@year %in_% params(qry)$year)

  if (is_empty(idx)) {
    return(x)
  }

  list(idx  = idx,
       year = obj@year[idx],
       id   = obj@identifier[idx],
       field = keys(obj)[idx])
}

#' @autoglobal
#' @noRd
match_query2 <- function(obj, qry) {

  x     <- select_years(obj, qry)
  qdx   <- set_names(seq_along(qry@params), names2(qry@params))
  param <- remove_year(qry)

  df <- purrr::imap(x$field, function(x, i) {
      cheapr::new_df(year = i, field = x)}) |>
      purrr::list_rbind() |>
      collapse::mtt(clean = clean_names(field)) |>
      collapse::sbt(clean %in_% names2(param)) |>
      collapse::mtt(qdx = qdx[clean])

  x <- list(
    idx   = x$idx,
    year  = x$year,
    id    = x$id,
    field = df)

  x$field$qdx <- unname(x$field$qdx)

  fd <- purrr::map(collapse::funique(x$field$year), function(yr) {
    set_names(
      qry@params[x$field[x$field$year == yr, ]$qdx],
      x$field[x$field$year == yr, ]$field)
    }) |>
    set_names(collapse::funique(x$field$year))

  list_modify(x, list(field = fd))
}
