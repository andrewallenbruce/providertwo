#' Standardize Query to Endpoint
#'
#' @param obj An `<endpoint>`, `<collection>` or `<group>` object.
#'
#' @param qry A `<query>` object.
#'
#' @returns A list of query parameters matched to an endpoint's fields.
#'
#' @examplesIf interactive()
#' standardize(
#'   endpoint("drug_state"),
#'   query2(
#'     year = 2022:2024,
#'     state = c("GA", "NY")
#'   )
#' )
#'
#' standardize(
#'   endpoint("enroll_prov"),
#'   query2(
#'     enrlmt_id = "I20040309000221")
#' )
#'
#' standardize(
#'   collection("dialysis"),
#'   query2(state = "AL")
#' )
#'
#' standardize(
#'   endpoint("dial_facility"),
#'   query2(
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
#'   query2(
#'     year = 2021:2023,
#'     practice_state_or_us_territory = c("GA", "FL"),
#'     practice_size = less_than(10, or_equal = TRUE),
#'     or("practice_state_or_us_territory",
#'        "practice_size")
#'   )
#' )
#'
#' standardize(
#'   endpoint("hc_quality"),
#'   query2(
#'     state = c("Georgia", "Alabama"),
#'     year = 2020:2023
#'   )
#' )
#'
#' standardize(
#'   endpoint("pdc_clinician"),
#'   query2(
#'     provider_first_name = starts_with("An"),
#'     provider_last_name = contains("JE"),
#'     state = c("CA", "GA", "NY")
#'   )
#' )
#'
#' @autoglobal
#' @export
standardize <- S7::new_generic("standardize", "obj", function(obj, qry) {
  check_class_query(qry)
  S7::S7_dispatch()
})

S7::method(standardize, class_group) <- function(obj, qry) {
  S7::prop(obj, "members") |>
    purrr::map(function(x) standardize(obj = x, qry = qry))
}

S7::method(standardize, class_catalog) <- function(obj, qry) {
  S7::prop(obj, "access") |> standardize(qry = qry)
}

S7::method(standardize, class_current) <- function(obj, qry) {

  pname <- rlang::names2(qry@params)
  clean <- field_switch(obj@fields)

  rlang::set_names(
      qry@params[qmatch(clean, pname)],
      obj@fields[sort(qmatch(pname, clean))])
}

S7::method(standardize, class_temporal) <- function(obj, qry) {

  x <- fastplyr::list_tidy(
    idx   = if (empty(qry@year)) seq_along(obj@year) else obj@year %iin% qry@year,
    year  = if (empty(idx)) obj@year else obj@year[idx],
    id    = if (empty(idx)) obj@identifier else obj@identifier[idx],
    field = if (empty(idx)) obj@fields@key else obj@fields@key[idx]
  )

  if (empty(qry@params)) {
    return(x)
  }

  df <- x$field |>
    purrr::map(\(x) cheapr::fast_df(field = x)) |>
    purrr::list_rbind(names_to = "year") |>
    collapse::mtt(clean = field_switch(field)) |>
    collapse::sbt(clean %iin% rlang::names2(qry@params)) |>
    collapse::mtt(qdx = unname(set_along(qry@params)[clean]))

  df <- x$year |>
    purrr::map(\(yr) rlang::set_names(
      qry@params[df[df$year == yr, ]$qdx],
      df[df$year == yr, ]$field)) |>
    rlang::set_names(x$year)

  cheapr::list_modify(x, list(field = df))
}
