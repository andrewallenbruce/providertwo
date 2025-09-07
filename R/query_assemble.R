#' Assemble a Query for an Endpoint
#'
#' @param obj An `<endpoint>`, `<collection>` or `<group>` object.
#'
#' @param qry A `<query>` object.
#'
#' @returns A list of query parameters matched to an endpoint's fields.
#'
#' @examples
#' assemble(
#'   endpoint("drug_state"),
#'   query(year = 2022:2024,
#'   state = any_of("GA", "NY")))
#'
#' assemble(
#'   endpoint("enroll_prov"),
#'   query(enrlmt_id = "I20040309000221"))
#'
#' assemble(
#'   collection("dialysis"),
#'   query(state = "AL"))
#'
#' assemble(
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
#' assemble(
#'   endpoint("qppe"),
#'   query(
#'     year = 2021:2023,
#'     practice_state_or_us_territory = any_of("GA", "FL"),
#'     practice_size = less_than(10, or_equal = TRUE)
#'   )
#' )
#'
#' assemble(
#'   endpoint("hc_quality"),
#'   query(state = any_of("Georgia", "Alabama"),
#'   year = 2020:2023))
#'
#' assemble(
#'   endpoint("pdc_clinician"),
#'   query(
#'     provider_first_name = starts_with("An"),
#'     provider_last_name = contains("JE"),
#'     state = any_of("CA", "GA", "NY")))
#'
#' @autoglobal
#' @export
assemble <- S7::new_generic("assemble", c("obj", "qry"), function(obj, qry) {
  if (!S7::S7_inherits(qry, class_query)) {
    cli::cli_abort(c("x" = paste0(
      "{.field qry} must be of {.cls class_query}, ",
      "not {.obj_type_friendly {qry}}."
    )),
    call = rlang::caller_env())
  }
  S7::S7_dispatch()
})

S7::method(assemble, list(class_group, class_query)) <- function(obj, qry) {
  S7::prop(obj, "members") |>
    purrr::map(function(x) assemble(obj = x, qry = qry))
}

S7::method(assemble, list(class_catalog, class_query)) <- function(obj, qry) {
  S7::prop(obj, "access") |> assemble(qry = qry)
}

S7::method(assemble, list(class_current, class_query)) <- function(obj, qry) {

  p <- match_query(obj, qry)

  if (rlang::is_empty(p)) {
    return(no_match_response(obj))
  }

  append_url(obj@identifier) |>
    collapse_query(generate_query(p))
}

S7::method(assemble, list(care_current, class_query)) <- function(obj, qry) {

  p <- match_query(obj, qry)

  if (rlang::is_empty(p)) {
    return(no_match_response(obj))
  }

  append_url(obj@identifier, "stats") |>
    collapse_query(generate_query(p, is_care = TRUE))
}

S7::method(assemble, list(class_temporal, class_query)) <- function(obj, qry) {

  p <- match_query2(obj, qry)

  if (identical("year", param_names(qry))) {
    return(year_only_response(p, obj))
  }

  qst <- purrr::map(p$field, function(x) {
    generate_query(x) |>
      unlist(use.names = FALSE) |>
      paste0(collapse = "&")
  }) |>
    unlist(use.names = FALSE)

  paste0(append_url(p$id), "&") |>
    glue::as_glue() +
    glue::as_glue(qst)
}

S7::method(assemble, list(care_temporal, class_query)) <- function(obj, qry) {

  p <- match_query2(obj, qry)

  if (identical("year", param_names(qry))) {
    return(year_only_response(p, obj))
  }

  qst <- purrr::map(p$field, function(x) {
    generate_query(x, is_care = TRUE) |>
      unlist(use.names = FALSE) |>
      paste0(collapse = "&")
  }) |>
    unlist(use.names = FALSE)

  paste0(append_url(p$id, "stats"), "&") |>
    glue::as_glue() +
    glue::as_glue(qst)
}
