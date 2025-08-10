#' @autoglobal
#' @noRd
select_years <- function(obj, qry) {

  if ("year" %!in_% names2(parameters(qry))) {

    x <- get_elem(obj@identifier, "^year$|^identifier$", regex = TRUE)
    return(set_names(x$identifier, x$year))

  } else {

    x <- sbt(obj@identifier, year %in_% parameters(qry)$year)

    if (is_empty(x)) {

      cli_noyears(obj, qry)
      x <- get_elem(obj@identifier, "^year$|^identifier$", regex = TRUE)

    }
  }
  set_names(x$identifier, x$year)
}

#' Build a Query for an Endpoint
#'
#' @param obj An `<endpoint>`, `<collection>` or `<group>` object.
#'
#' @param qry A `<query>` object.
#'
#' @returns A list of query parameters matched to an endpoint's fields.
#'
#' @examples
#' build(
#'   endpoint("drug_state"),
#'   query(
#'     year = 2022:2024,
#'     state = any_of(c("GA", "NY"))))
#'
#' build(endpoint("enroll_prov"),
#'       query(first_name = starts_with("And"),
#'       last_name = ends_with("ce")))
#'
#' build(endpoint("dial_facility"), query(city = "BIRMINGHAM"))
#'
#' build(collection("dialysis"), query(state = "AL"))
#'
#' build(
#'   endpoint("quality_payment"),
#'   query(
#'     year = 2021:2023,
#'     practice_state_or_us_territory = any_of(c("GA", "FL")),
#'     allowed_charges = less_than(10000)))
#'
#' build(
#'   endpoint("pdc_clinician"),
#'   query(
#'     provider_first_name = starts_with("An"),
#'     provider_last_name = contains("J"),
#'     state = any_of(c("CA", "GA", "NY"))))
#'
#' @autoglobal
#' @export
build <- S7::new_generic("build", c("obj", "qry"), function(obj, qry) {
  S7::S7_dispatch()
})

S7::method(build, list(class_group, class_query)) <- function(obj, qry) {
  S7::prop(obj, "members") |> purrr::map(\(x) build(obj = x, qry = qry))
}

S7::method(build, list(class_catalog, class_query)) <- function(obj, qry) {
  S7::prop(obj, "access") |> build(qry = qry)
}

S7::method(build, list(class_current, class_query)) <- function(obj, qry) {

  prm <- match_query(obj, qry)

  if (is_empty(prm)) {
    cli_nomatch(obj)
    return(
      class_results(
        alias  = meta(obj)$alias,
        title  = meta(obj)$title,
        base   = obj@identifier,
        total  = obj@dimensions@total,
        found  = 0L,
        limit  = obj@dimensions@limit
      )
    )
  }

  prm <- generate_query(prm)
  url <- append_url(obj@identifier) |> collapse_query(prm)
  res <- map(url, request) |>
    req_perform_parallel(on_error = "continue") |>
    map(function(x)
      parse_string(x, query = "count")) |>
    unlist(use.names = FALSE)

  class_results(
    alias  = meta(obj)$alias,
    title  = meta(obj)$title,
    params = names2(prm),
    base   = url,
    total  = obj@dimensions@total,
    found  = res %||% 0L,
    limit  = obj@dimensions@limit
  )
}

S7::method(build, list(class_temporal, class_query)) <- function(obj, qry) {

  url <- select_years(obj, qry)
  prm <- match_query(obj, qry)
  prm <- generate_query(prm)
  url <- map(url, \(x) append_url(x) |> collapse_query(prm))

  res <- map(url, request) |>
    req_perform_parallel(on_error = "continue") |>
    map(function(x) parse_string(x, query = "count")) |>
    unlist(use.names = FALSE)

  class_results(
    alias  = meta(obj)$alias,
    title  = meta(obj)$title,
    params = names2(prm),
    base   = url,
    total  = obj@dimensions@total,
    found  = res,
    limit  = obj@dimensions@limit
  )
}

S7::method(build, list(care_current, class_query)) <- function(obj, qry) {

  prm <- match_query(obj, qry)

  if (is_empty(prm)) {
    cli_nomatch(obj)
    return(
      class_results(
        alias  = meta(obj)$alias,
        title  = meta(obj)$title,
        base   = obj@identifier,
        total  = obj@dimensions@total,
        found  = 0L,
        limit  = obj@dimensions@limit
      )
    )
  }

  prm <- generate_query(prm, is_care = TRUE)
  url <- append_url(obj@identifier, "stats") |>
    collapse_query(prm)

  res <- map(url, request) |>
    req_perform_parallel(on_error = "continue") |>
    map(function(x)
      parse_string(x)) |>
    yank("data")

  class_results(
    alias  = meta(obj)$alias,
    title  = meta(obj)$title,
    params = names2(prm),
    base   = url,
    total  = res$total_rows %||% 0L,
    found  = res$found_rows %||% 0L,
    limit  = obj@dimensions@limit
  )
}

S7::method(build, list(care_temporal, class_query)) <- function(obj, qry) {

  url <- select_years(obj, qry)
  prm <- match_query(obj, qry)
  prm <- generate_query(prm, is_care = TRUE)
  url <- map(url, \(x) append_url(x, "stats") |>
               collapse_query(prm))

  res <- map(url, request) |>
    req_perform_parallel(on_error = "continue") |>
    map(function(x) parse_string(x))

  class_results(
    alias  = meta(obj)$alias,
    title  = meta(obj)$title,
    params = names2(prm),
    base   = url,
    total  = get_elem(res, "total_rows") |> unlist(use.names = FALSE),
    found  = get_elem(res, "found_rows") |> unlist(use.names = FALSE),
    limit  = obj@dimensions@limit
  )
}
