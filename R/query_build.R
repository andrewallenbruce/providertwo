#' @autoglobal
#' @noRd
select_years <- function(obj, qry) {
  x <- list(
    idx  = seq_along(obj@year),
    year = obj@year,
    id   = obj@identifier
  )

  if ("year" %!in_% names2(params(qry)))
    return(x)

  idx <- which_(obj@year %in_% params(qry)$year)

  if (is_empty(idx))
    cli_noyears(obj, qry)
  return(x)

  list(idx  = idx,
       year = obj@year[idx],
       id   = obj@identifier[idx])
}
# build(endpoint("drug_state"), query(year = 2022:2024, state = any_of(c("GA", "NY"))))
#' Build a Query for an Endpoint
#'
#' @param obj An `<endpoint>`, `<collection>` or `<group>` object.
#'
#' @param qry A `<query>` object.
#'
#' @returns A list of query parameters matched to an endpoint's fields.
#'
#' @examples
#' build(endpoint("enroll_prov"), query(enrlmt_id = "I20040309000221"))
#' build(endpoint("enroll_prov"), query(npi = 1417918293))
#' build(endpoint("enroll_prov"), query(pecos_asct_cntl_id = 2860305554))
#' build(endpoint("dial_facility"), query(city = "BIRMINGHAM"))
#' build(collection("dialysis"), query(state = "AL"))
#'
#' build(
#'   endpoint("qppe"),
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
      class_response(
        alias  = meta(obj)$alias,
        title  = meta(obj)$title,
        year   = as.integer(substr(obj@metadata$modified, 1, 4)),
        string = obj@identifier,
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

  class_response(
    alias  = meta(obj)$alias,
    title  = meta(obj)$title,
    param  = names2(prm),
    year   = as.integer(substr(obj@metadata$modified, 1, 4)),
    string = url,
    total  = obj@dimensions@total,
    found  = res %||% 0L,
    limit  = obj@dimensions@limit
  )
}

S7::method(build, list(class_temporal, class_query)) <- function(obj, qry) {

  yrl <- select_years(obj, qry)
  prm <- match_query(obj, qry)
  prm <- generate_query(prm)
  url <- map(yrl$id, \(x) append_url(x) |> collapse_query(prm))

  res <- map(url, request) |>
    req_perform_parallel(on_error = "continue") |>
    map(function(x) parse_string(x, query = "count")) |>
    unlist(use.names = FALSE)

  class_response(
    alias  = meta(obj)$alias,
    title  = meta(obj)$title,
    param  = names2(prm),
    year   = as.integer(yrl$year),
    string = unlist(url, use.names = FALSE),
    total  = obj@dimensions@total[yrl$idx],
    found  = res,
    limit  = obj@dimensions@limit
  )
}

S7::method(build, list(care_current, class_query)) <- function(obj, qry) {

  prm <- match_query(obj, qry)

  if (is_empty(prm)) {
    cli_nomatch(obj)
    return(
      class_response(
        alias  = meta(obj)$alias,
        title  = meta(obj)$title,
        year   = as.integer(substr(obj@metadata$modified, 1, 4)),
        string = obj@identifier,
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

  class_response(
    alias  = meta(obj)$alias,
    title  = meta(obj)$title,
    param  = names2(prm),
    year   = as.integer(substr(obj@metadata$modified, 1, 4)),
    string = url,
    total  = res$total_rows %||% 0L,
    found  = res$found_rows %||% 0L,
    limit  = obj@dimensions@limit
  )
}

S7::method(build, list(care_temporal, class_query)) <- function(obj, qry) {

  yrl <- select_years(obj, qry)
  prm <- match_query(obj, qry)
  prm <- generate_query(prm, is_care = TRUE)
  url <- map(yrl$id, \(x) append_url(x, "stats") |> collapse_query(prm))

  res <- map(url, request) |>
    req_perform_parallel(on_error = "continue") |>
    map(function(x) parse_string(x))

  class_response(
    alias  = meta(obj)$alias,
    title  = meta(obj)$title,
    param  = names2(prm),
    year   = as.integer(yrl$year),
    string = unlist(url, use.names = FALSE),
    total  = get_elem(res, "total_rows") |> unlist(use.names = FALSE),
    found  = get_elem(res, "found_rows") |> unlist(use.names = FALSE),
    limit  = obj@dimensions@limit
  )
}
