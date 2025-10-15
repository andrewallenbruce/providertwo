#' Assemble a Query for an Endpoint
#'
#' @param obj An `<endpoint>`, `<collection>` or `<group>` object.
#'
#' @param qry A `<query>` object.
#'
#' @returns A list of query parameters matched to an endpoint's fields.
#'
#' @examplesIf interactive()
#'
#' assemble(
#'   endpoint("drug_state"),
#'   query2(
#'     year = 2022:2024,
#'     state = c("GA", "NY")
#'   )
#' )
#'
#' assemble(
#'   endpoint("enroll_prov"),
#'   query2(enrlmt_id = "I20040309000221"))
#'
#' assemble(
#'   collection("dialysis"),
#'   query2(state = "AL"))
#'
#' assemble(
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
#' assemble(
#'   endpoint("qppe"),
#'   query2(
#'     year = 2021:2023,
#'     practice_state_or_us_territory = c("GA", "FL"),
#'     practice_size = less_than(10, or_equal = TRUE)
#'   )
#' )
#'
#' assemble(
#'   endpoint("hc_quality"),
#'   query2(state = c("Georgia", "Alabama"),
#'   year = 2020:2023))
#'
#' assemble(
#'   endpoint("pdc_clinician"),
#'   query2(
#'     provider_first_name = starts_with("An"),
#'     provider_last_name = contains("JE"),
#'     state = c("CA", "GA", "NY")))
#'
#' @autoglobal
#' @export
assemble <- S7::new_generic("assemble", "obj", function(obj, qry) {
  check_class_query(qry)
  S7::S7_dispatch()
})

S7::method(assemble, class_group) <- function(obj, qry) {
  S7::prop(obj, "members") |>
    purrr::map(function(x) assemble(obj = x, qry = qry))
}

S7::method(assemble, class_catalog) <- function(obj, qry) {
  S7::prop(obj, "access") |> assemble(qry = qry)
}

S7::method(assemble, class_current) <- function(obj, qry) {

  p <- match_query_G(obj, qry)

  if (empty(p)) {
    return(obj@identifier)
  }

  append_url(obj@identifier) |>
    collapse_query(
      cheapr::list_combine(
        query_def_GRP(p),
        query_def_ARG(p)) |>
        purrr::list_c())
}

S7::method(assemble, care_current) <- function(obj, qry) {

  p <- match_query_G(obj, qry)

  if (empty(p)) {
    return(obj@identifier)
  }

  append_url(obj@identifier, "stats") |>
    collapse_query(
      cheapr::list_combine(
        query_care_GRP(p),
        query_care_ARG(p)) |>
        purrr::list_c())
}

S7::method(assemble, class_temporal) <- function(obj, qry) {

  if (empty(qry@params)) {
    return(select_years_2G(obj, qry)$id)
  }

  p <- match_query_2G(obj, qry)

  qst <- purrr::map(p$field, function(x) {
    generate_query(x) |>
      unlist(use.names = FALSE) |>
      paste0(collapse = "&")
  }) |>
    unlist(use.names = FALSE)

  paste0(append_url(p$id), "&") |>
    glue::as_glue() +
    glue::as_glue(qst) |>
    as.character()
}

S7::method(assemble, care_temporal) <- function(obj, qry) {

  if (empty(qry@params)) {
    return(select_years_2G(obj, qry)$id)
  }

  p <- match_query_2G(obj, qry)

  qst <- purrr::map(p$field, function(x) {
    generate_query(x, is_care = TRUE) |>
      unlist(use.names = FALSE) |>
      paste0(collapse = "&")
  }) |>
    unlist(use.names = FALSE)

  paste0(append_url(p$id, "stats"), "&") |>
    glue::as_glue() +
    glue::as_glue(qst) |>
    as.character()
}
