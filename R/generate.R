#' Generate a Query for an Endpoint
#'
#' @name generate
#'
#' @param obj An `<endpoint>`, `<collection>` or `<group>` object.
#'
#' @param qry A `<query>` object.
#'
#' @returns A list of query parameters matched to an endpoint's fields.
#'
#' @examples
#' generate(
#'   endpoint("drug_state"),
#'   query2(year = 2022:2024,
#'   state = c("GA", "NY")))
#'
#' generate(
#'   endpoint("enroll_prov"),
#'   query2(enrlmt_id = "I20040309000221"))
#'
#' generate(
#'   collection("dialysis"),
#'   query2(state = "AL"))
#'
#' generate(
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
#' generate(
#'   endpoint("qppe"),
#'   query2(
#'     year = 2021:2023,
#'     practice_state_or_us_territory = c("GA", "FL"),
#'     practice_size = less_than(10, or_equal = TRUE)
#'   )
#' )
#'
#' generate(
#'   endpoint("hc_quality"),
#'   query2(state = c("Georgia", "Alabama"),
#'   year = 2020:2023))
#'
#' generate(
#'   endpoint("pdc_clinician"),
#'   query2(
#'     provider_first_name = starts_with("An"),
#'     provider_last_name = contains("JE"),
#'     state = c("CA", "GA", "NY")))
#'
#' @autoglobal
#' @export
generate %:=% S7::new_generic("obj", function(obj, qry) {
  check_class_query(qry)
  S7::S7_dispatch()
})

S7::method(generate, class_group) <- function(obj, qry) {
  S7::prop(obj, "members") |>
    purrr::map(function(x) generate(obj = x, qry = qry))
}

S7::method(generate, class_catalog) <- function(obj, qry) {
  S7::prop(obj, "access") |> generate(qry = qry)
}

S7::method(generate, class_current) <- function(obj, qry) {
  x <- standardize(obj, qry)

  if (!empty(x$group)) {
    x$group <- x$group |>
      purrr::imap(function(x, N)
        paste0("conditions[", N, "][conjunction]=", x)) |>
      unname() |>
      purrr::map_chr(paste0, collapse = "&")
  }

  x$field <- x$field |>
    purrr::imap(function(x, N) {
      c(place_member_of2(x),
        place_path2(N),
        place_operator2(x),
        place_value2(x))
    }) |>
    unname() |>
    purrr::imap(function(x, idx)
      gsub(
        x           = x,
        pattern     = "<<i>>",
        replacement = idx,
        fixed       = TRUE
      )) |>
    purrr::map_chr(paste0, collapse = "&")

  cheapr::cheapr_c(x$group, x$field)
}

S7::method(generate, care_current) <- function(obj, qry) {
  x <- standardize(obj, qry)

  if (!empty(x$group)) {
    x$group <- x$group |>
      purrr::imap(function(x, N)
        paste0("filter[", N, "][group][conjunction]=", x)) |>
      unname() |>
      purrr::map_chr(paste0, collapse = "&")
  }

  x$field <- x$field |>
    purrr::imap(function(x, N) {
      c(place_member_of(x),
        place_path(N),
        place_operator(x),
        place_value(x))
    }) |>
    unname() |>
    purrr::imap(function(x, idx)
      gsub(
        x           = x,
        pattern     = "<<i>>",
        replacement = idx,
        fixed       = TRUE
      )) |>
    purrr::map_chr(paste0, collapse = "&")

  cheapr::cheapr_c(x$group, x$field)
}

S7::method(generate, class_temporal) <- function(obj, qry) {

  x <- standardize(obj, qry)

  if (!empty(x$group)) {
    group <- x$group |>
      collapse::get_elem("^junc", regex = TRUE) |>
      rlang::set_names(\(x) gsub("junc_", "g",  x)) |>
      purrr::imap(function(x, N)
        paste0("conditions[", N, "][conjunction]=", x))

    grp_yrs <- x$group |>
      collapse::get_elem("^year", regex = TRUE) |>
      rlang::set_names(\(x) gsub("year_", "g",  x))
  }

  .fun <- function(x) {
    purrr::imap(x, function(x, N) {
      c(place_member_of2(x),
        place_path2(N),
        place_operator2(x),
        place_value2(x))
    }) |>
      unname() |>
      purrr::imap(function(x, idx)
        gsub(
          x           = x,
          pattern     = "<<i>>",
          replacement = idx,
          fixed       = TRUE
        )) |>
      purrr::map_chr(paste0, collapse = "&")
  }

  x$field <- purrr::map(x$field, .fun)

  cheapr::list_combine(
    if (!empty(x$group)) group else NULL,
    if (!empty(x$group)) grp_yrs else NULL,
    x$field
    )

}
