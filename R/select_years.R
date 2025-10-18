#' Standardize Query to Endpoint
#'
#' @param obj An `<endpoint>`, `<collection>` or `<group>` object.
#'
#' @param qry A `<query>` object.
#'
#' @returns A list of query parameters matched to an endpoint's fields.
#'
#' @examples
#' select_years(
#'   endpoint("drug_state"),
#'   query2(
#'     year = 2022:2024,
#'     state = c("GA", "NY")
#'   )
#' )
#'
#' select_years(
#'   endpoint("dial_facility"),
#'   query2(
#'     state = "GA",
#'     city = "Atlanta",
#'     provider_name = starts_with("C")
#'   )
#' )
#'
#' select_years(
#'   endpoint("qppe"),
#'   query2(
#'     year = 2021:2023,
#'     state = c("GA", "FL"),
#'     practice_size = less_than(10, or_equal = TRUE)
#'   )
#' )
#'
#' select_years(
#'   endpoint("hc_quality"),
#'   query2(
#'     state = c("Georgia", "Alabama"),
#'     year = 2020:2023
#'   )
#' )
#'
#' @autoglobal
#' @export
select_years <- S7::new_generic("select_years", "obj", function(obj, qry) {
  check_class_query(qry)
  S7::S7_dispatch()
})

S7::method(select_years, class_group) <- function(obj, qry) {
  S7::prop(obj, "members") |>
    purrr::map(function(x) select_years(obj = x, qry = qry))
}

S7::method(select_years, class_catalog) <- function(obj, qry) {
  S7::prop(obj, "access") |> select_years(qry = qry)
}

S7::method(select_years, class_current) <- function(obj, qry) {
  cli::cli_alert_warning(c("Skipping {.obj_type_friendly {obj}}"))
  return()
}

S7::method(select_years, class_temporal) <- function(obj, qry) {
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
