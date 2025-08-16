#' @autoglobal
#' @noRd
total_rows <- function(x) {
  get_elem(x, "total_rows") |>
    unlist(use.names = FALSE)
}

#' @autoglobal
#' @noRd
found_rows <- function(x) {
  get_elem(x, "found_rows") |>
    unlist(use.names = FALSE)
}

#' @autoglobal
#' @noRd
no_match_response <- function(obj) {
  class_response(
    alias  = meta(obj)$alias,
    title  = meta(obj)$title,
    year   = date_year(meta(obj)$modified),
    string = obj@identifier,
    total  = obj@dimensions@total,
    found  = 0L,
    limit  = obj@dimensions@limit
  )
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
#' build(
#'   endpoint("enroll_prov"),
#'   query(enrlmt_id = "I20040309000221"))
#'
#' build(
#'   collection("dialysis"),
#'   query(state = "AL"))
#'
#' build(
#'   endpoint("dial_facility"),
#'   query(
#'     city = "BIRMINGHAM",
#'     provcity = "BIRMINGHAM")
#'   )
#'
#' build(
#'   endpoint("qppe"),
#'   query(
#'     year = 2021:2023,
#'     practice_state_or_us_territory = any_of(c("GA", "FL")),
#'     practice_size = less_than(10, or_equal = TRUE)
#'   )
#' )
#'
#' build(
#'   endpoint("hc_quality"),
#'   query(
#'     state = any_of(c("Georgia", "Alabama")),
#'     year = 2020:2023
#'    )
#'  )
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
  if (!S7::S7_inherits(qry, class_query)) {
    cli::cli_abort(c("x" = paste0(
      "{.field qry} must be of {.cls class_query}, ",
      "not {.obj_type_friendly {qry}}."
    )),
    call = caller_env())
    }
  S7::S7_dispatch()
})

S7::method(build, list(class_group, class_query)) <- function(obj, qry) {
  S7::prop(obj, "members") |> purrr::map(function(x) build(obj = x, qry = qry))
}

S7::method(build, list(class_catalog, class_query)) <- function(obj, qry) {
  S7::prop(obj, "access") |> build(qry = qry)
}

S7::method(build, list(class_current, class_query)) <- function(obj, qry) {

  prm <- match_query(obj, qry)

  if (is_empty(prm)) {
    cli_nomatch(obj)
    return(no_match_response(obj))
  }

  prm <- generate_query(prm)
  url <- append_url(obj@identifier) |> collapse_query(prm)
  res <- map_perform_parallel(url, query = "count") |>
    unlist(use.names = FALSE)

  class_response(
    alias  = meta(obj)$alias,
    title  = meta(obj)$title,
    param  = names2(prm),
    year   = date_year(meta(obj)$modified),
    string = url,
    total  = dims(obj)@total,
    found  = res %||% 0L,
    limit  = dims(obj)@limit
  )
}

S7::method(build, list(care_current, class_query)) <- function(obj, qry) {

  prm <- match_query(obj, qry)

  if (is_empty(prm)) {
    cli_nomatch(obj)
    return(no_match_response(obj))
  }

  prm <- generate_query(prm, is_care = TRUE)
  url <- append_url(obj@identifier, "stats") |>
    collapse_query(prm)

  res <- map_perform_parallel(url) |>
    yank("data")

  class_response(
    alias  = meta(obj)$alias,
    title  = meta(obj)$title,
    param  = names2(prm),
    year   = date_year(meta(obj)$modified),
    string = url,
    total  = total_rows(res) %||% 0L,
    found  = found_rows(res) %||% 0L,
    limit  = dims(obj)@limit
  )
}

S7::method(build, list(class_temporal, class_query)) <- function(obj, qry) {

  x <- match_query2(obj, qry)
  p <- finalize_match2(x$field)

  qst <- map(p, function(x) {
    generate_query(x) |>
      unlist(use.names = FALSE) |>
      paste0(collapse = "&")
  }) |>
    unlist(use.names = FALSE)

  url <- paste0(append_url(x$id), "&")
  url <- glue::as_glue(url) + glue::as_glue(qst)

  res <- map_perform_parallel(url, query = "count") |>
    unlist(use.names = FALSE)

  class_response(
    alias  = meta(obj)$alias,
    title  = meta(obj)$title,
    param  = map(p, names2),
    year   = as.integer(x$year),
    string = as.character(url),
    total  = dims(obj)@total[x$idx],
    found  = res,
    limit  = dims(obj)@limit
  )
}

S7::method(build, list(care_temporal, class_query)) <- function(obj, qry) {

  x <- match_query2(obj, qry)
  p <- finalize_match2(x$field)

  qst <- map(p, function(x) {
    generate_query(x, is_care = TRUE) |>
      unlist(use.names = FALSE) |>
      paste0(collapse = "&")
  }) |>
    unlist(use.names = FALSE)

  url <- paste0(append_url(x$id, "stats"), "&")
  url <- glue::as_glue(url) + glue::as_glue(qst)

  res <- map_perform_parallel(url)

  class_response(
    alias  = meta(obj)$alias,
    title  = meta(obj)$title,
    param  = map(p, names2),
    year   = as.integer(x$year),
    string = as.character(url),
    total  = total_rows(res),
    found  = found_rows(res),
    limit  = dims(obj)@limit
  )
}
