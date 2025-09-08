#' Standardize a Query for an Endpoint
#'
#' @param obj An `<endpoint>`, `<collection>` or `<group>` object.
#'
#' @param qry A `<query>` object.
#'
#' @returns A list of query parameters matched to an endpoint's fields.
#'
#' @examples
#' standardize(
#'   endpoint("drug_state"),
#'   query(year = 2022:2024,
#'   state = any_of("GA", "NY")))
#'
#' standardize(
#'   endpoint("enroll_prov"),
#'   query(enrlmt_id = "I20040309000221"))
#'
#' standardize(
#'   collection("dialysis"),
#'   query(state = "AL"))
#'
#' standardize(
#'   endpoint("dial_facility"),
#'   query(
#'     state = "GA",
#'     city = "Atlanta",
#'     provcity = "Atlanta",
#'     provider_name = starts_with("C"),
#'     provname = starts_with("C")
#'   )
#' )
#'
#' standardize(
#'   endpoint("qppe"),
#'   query(
#'     year = 2021:2023,
#'     practice_state_or_us_territory = any_of("GA", "FL"),
#'     practice_size = less_than(10, or_equal = TRUE)
#'   )
#' )
#'
#' standardize(
#'   endpoint("hc_quality"),
#'   query(state = any_of("Georgia", "Alabama"), year = 2020:2023))
#'
#' standardize(
#'   endpoint("pdc_clinician"),
#'   query(
#'     provider_first_name = starts_with("An"),
#'     provider_last_name = contains("JE"),
#'     state = any_of("CA", "GA", "NY")))
#'
#' @autoglobal
#' @export
standardize <- S7::new_generic("standardize", c("obj", "qry"), function(obj, qry) {
  if (!S7::S7_inherits(qry, class_query)) {
    cli::cli_abort(c("x" = paste0(
      "{.field qry} must be of {.cls class_query}, ",
      "not {.obj_type_friendly {qry}}."
    )),
    call = rlang::caller_env())
  }
  S7::S7_dispatch()
})

S7::method(standardize, list(class_group, class_query)) <- function(obj, qry) {
  S7::prop(obj, "members") |>
    purrr::map(function(x) standardize(obj = x, qry = qry))
}

S7::method(standardize, list(class_catalog, class_query)) <- function(obj, qry) {
  S7::prop(obj, "access") |> standardize(qry = qry)
}

S7::method(standardize, list(class_current, class_query)) <- function(obj, qry) {

  param   <- not_year(qry)
  p_name  <- rlang::names2(param)
  field   <- keys(obj)
  clean   <- clean_names(field)

  rlang::set_names(
    param[qmatch(clean, p_name)],
    field[sort(qmatch(p_name, clean))])
}

S7::method(standardize, list(class_temporal, class_query)) <- function(obj, qry) {

  x <- list(
    idx   = seq_along(obj@year),
    year  = obj@year,
    id    = obj@identifier,
    field = keys(obj))

  if ("year" %in_% param_names(qry)) {
    idx <- cheapr::which_(obj@year %in_% params(qry)$year)

    if (!rlang::is_empty(idx)) {
      x <- list(
        idx   = idx,
        year  = obj@year[idx],
        id    = obj@identifier[idx],
        field = keys(obj)[idx]
      )
    }
  }

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
