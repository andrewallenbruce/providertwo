#' @autoglobal
#' @noRd
total_rows <- function(x) {
  collapse::get_elem(x, "total_rows") |>
    unlist(use.names = FALSE)
}

#' @autoglobal
#' @noRd
found_rows <- function(x) {
  collapse::get_elem(x, "found_rows") |>
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
    total  = dims(obj)@total,
    found  = 0L,
    limit  = dims(obj)@limit
  )
}

#' @autoglobal
#' @noRd
year_only_response <- function(p, obj) {
  class_response(
    alias  = meta(obj)$alias,
    title  = meta(obj)$title,
    param  = p$field,
    year   = as.integer(p$year),
    string = p$id,
    total  = dims(obj)@total[p$idx],
    found  = dims(obj)@total[p$idx],
    limit  = dims(obj)@limit
  )
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
#'   query(year = 2022:2024,
#'   state = any_of("GA", "NY")))
#'
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
#'     state = "GA",
#'     city = "Atlanta",
#'     provcity = "Atlanta",
#'     provider_name = starts_with("C"),
#'     provname = starts_with("C")
#'   )
#' )
#'
#' build(
#'   endpoint("qppe"),
#'   query(
#'     year = 2021:2023,
#'     practice_state_or_us_territory = any_of("GA", "FL"),
#'     practice_size = less_than(10, or_equal = TRUE)
#'   )
#' )
#'
#' build(
#'   endpoint("hc_quality"),
#'   query(state = any_of("Georgia", "Alabama"),
#'   year = 2020:2023))
#'
#' build(
#'   endpoint("pdc_clinician"),
#'   query(
#'     provider_first_name = starts_with("An"),
#'     provider_last_name = contains("JE"),
#'     state = any_of("CA", "GA", "NY")))
#'
#' @autoglobal
#' @export
build <- S7::new_generic("build", c("obj", "qry"), function(obj, qry) {
  if (!S7::S7_inherits(qry, class_query)) {
    cli::cli_abort(c("x" = paste0(
      "{.field qry} must be of {.cls class_query}, ",
      "not {.obj_type_friendly {qry}}."
    )),
    call = rlang::caller_env())
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

  if (rlang::is_empty(prm)) {
    cli_no_match(obj)
    return(no_match_response(obj))
  }

  prm <- generate_query(prm)
  url <- append_url(obj@identifier) |>
    collapse_query(prm)

  res <- map_perform_parallel(url, query = "count") |>
    unlist(use.names = FALSE)

  class_response(
    alias  = meta(obj)$alias,
    title  = meta(obj)$title,
    param  = rlang::names2(prm),
    year   = date_year(meta(obj)$modified),
    string = url,
    total  = dims(obj)@total,
    found  = res %||% 0L,
    limit  = dims(obj)@limit
  )
}

S7::method(build, list(care_current, class_query)) <- function(obj, qry) {

  prm <- match_query(obj, qry)

  if (rlang::is_empty(prm)) {
    cli_no_match(obj)
    return(no_match_response(obj))
  }

  prm <- generate_query(prm, is_care = TRUE)
  url <- append_url(obj@identifier, "stats") |>
    collapse_query(prm)

  res <- map_perform_parallel(url) |> yank("data")

  class_response(
    alias  = meta(obj)$alias,
    title  = meta(obj)$title,
    param  = rlang::names2(prm),
    year   = date_year(meta(obj)$modified),
    string = url,
    total  = total_rows(res) %||% 0L,
    found  = found_rows(res) %||% 0L,
    limit  = dims(obj)@limit
  )
}

S7::method(build, list(class_temporal, class_query)) <- function(obj, qry) {

  p <- match_query2(obj, qry)

  if (identical("year", param_names(qry))) {
    cli_yr_only(obj)
    return(year_only_response(p, obj))
  }

  qst <- purrr::map(p$field, function(x) {
    generate_query(x) |>
      unlist(use.names = FALSE) |>
      paste0(collapse = "&")
  }) |>
    unlist(use.names = FALSE)

  url <- paste0(append_url(p$id), "&")
  # url <- append_url(p$id)
  url <- glue::as_glue(url) + glue::as_glue(qst)

  res <- map_perform_parallel(url, query = "count") |>
    unlist(use.names = FALSE)

  cli_sum_found(res,
                dims(obj)@total[p$idx],
                purrr::map_int(res, offset, limit = dims(obj)@limit))

  class_response(
    alias  = meta(obj)$alias,
    title  = meta(obj)$title,
    param  = purrr::map(p$field, rlang::names2),
    year   = as.integer(p$year),
    string = as.character(url),
    total  = dims(obj)@total[p$idx],
    found  = res,
    limit  = dims(obj)@limit
  )
}

S7::method(build, list(care_temporal, class_query)) <- function(obj, qry) {

  p <- match_query2(obj, qry)

  if (identical("year", param_names(qry))) {
    cli_yr_only(obj)
    return(year_only_response(p, obj))
  }

  qst <- purrr::map(p$field, function(x) {
    generate_query(x, is_care = TRUE) |>
      unlist(use.names = FALSE) |>
      paste0(collapse = "&")
  }) |>
    unlist(use.names = FALSE)

  url <- paste0(append_url(p$id, "stats"), "&")
  url <- glue::as_glue(url) + glue::as_glue(qst)

  res <- map_perform_parallel(url)

  cli_sum_found(
    found_rows(res),
    total_rows(res),
    purrr::map_int(found_rows(res), offset, limit = dims(obj)@limit)
  )

  class_response(
    alias  = meta(obj)$alias,
    title  = meta(obj)$title,
    param  = purrr::map(p$field, rlang::names2),
    year   = as.integer(p$year),
    string = as.character(url),
    total  = total_rows(res),
    found  = found_rows(res),
    limit  = dims(obj)@limit
  )
}
