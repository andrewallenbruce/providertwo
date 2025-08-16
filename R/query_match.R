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
select_years <- function(obj, qry) {
  x <- list(
    idx  = seq_along(obj@year),
    year = obj@year,
    id   = obj@identifier,
    field = keys(obj))

  if ("year" %!in_% names2(params(qry))) {
    return(x)
  }

  idx <- which_(obj@year %in_% params(qry)$year)

  if (is_empty(idx)) {
    cli_noyears(obj, qry)
    return(x)
  }

  list(idx  = idx,
       year = obj@year[idx],
       id   = obj@identifier[idx],
       field = keys(obj)[idx])
}

#' @autoglobal
#' @noRd
match_query_temporal <- function(obj, qry) {

  x <- select_years(obj, qry)
  param <- not_year(qry)

  df <- collapse::join(
    purrr::imap(x$field, function(x, i) {
      fastplyr::new_tbl(year = i, field = x)
    }) |>
      purrr::list_rbind() |>
      collapse::mtt(clean = clean_names(field)) |>
      collapse::sbt(clean %in_% names2(param)),

    fastplyr::new_tbl(clean = names2(param), param = unname(param)),
    on = "clean",
    multiple = TRUE,
    verbose = 0L
  ) |>
    collapse::slt(-clean)

  list(
    idx   = x$idx,
    year  = x$year,
    id    = x$id,
    param = df)
}

#' @autoglobal
#' @noRd
c_match <- function(a, b) {
  collapse::fmatch(x = a, table = b, nomatch = 0L, overid = 2)
}

#' @autoglobal
#' @noRd
not_year <- function(x) {
  params(x)[names2(params(x)) %!=% "year"]
}

#' @autoglobal
#' @noRd
map_match_query <- function(obj, qry) {

  param  <- not_year(qry)
  pname  <- rlang::names2(param)

  # TODO Use a modifier on "year"
  # parameter if meant for an API field?
  field <- keys(obj)
  clean <- purrr::map(field, clean_names)

  fin <- purrr::pmap(
    list(clean, field),
    function(cl, fl) {
      rlang::set_names(cl, fl)[c_match(pname, rlang::set_names(cl, fl))]
    }) |>
    purrr::compact()

  imap(fin, \(x, i) {
    param[names(param) %in% unlist(x, use.names = FALSE)]
  }) |>
    purrr::map2(fin, \(x, y) set_names(x, names2(y)))
}

#' @autoglobal
#' @noRd
match_query <- function(obj, qry) {

  param  <- not_year(qry)
  pname  <- names2(param)

  # TODO Use a modifier on "year"
  # parameter if meant for an API field?
  field <- keys(obj) |>
    unlist(use.names = FALSE) |>
    collapse::funique()

  clean <- clean_names(field)

  set_names(
    param[c_match(clean, pname)],
    field[sort(c_match(pname, clean))])
}

#' @autoglobal
#' @noRd
match_query2 <- function(obj, qry) {

  param <- not_year(qry)

  k <- keys(obj) |>
    unlist(use.names = FALSE) |>
    collapse::funique()

  vec <- set_names(k, clean_names(k))

  raw <- vec[names2(vec) %in_% names2(param)]

  values <- cheapr::cheapr_rep_each(
    param,
    cheapr::counts(
      names(raw))$count)

  named_args <- set_names(values, unname(raw))

  list(
    value = named_args,
    year = map(keys(obj), \(x) x[c_match(unname(raw), x)]))
}
