#' @autoglobal
#' @noRd
tidy_resources <- function(x) {
  x |>
    collapse::mtt(
      year     = extract_year(name),
      file     = rm_space(gremove(name, " [0-9]{4}|[0-9]{4} ")),
      size     = fs::as_fs_bytes(fileSize),
      ext      = tolower(fs::path_ext(downloadURL)),
      download = downloadURL,
      .keep = c("year", "file", "size", "ext")) |>
    fastplyr::f_fill(year) |>
    fastplyr::as_tbl() |>
    collapse::mtt(year = ifelse(is.na(year), extract_year(download), year))
}

#' List resources
#'
#' @param obj `<class_care>` object
#'
#' @returns A list of available API resources.
#'
#' @examples
#' list_resources(endpoint("enroll_prov"))
#' list_resources(group("asc_facility", "enterprise", "lab_fee"))
#' @autoglobal
#' @export
list_resources <- S7::new_generic("list_resources", "obj", function(obj) {
  S7::S7_dispatch()
})

S7::method(list_resources, class_group) <- function(obj) {
  S7::prop(obj, "members") |> purrr::map(list_resources)
}

S7::method(list_resources, class_catalog) <- function(obj) {
  cli::cli_alert_warning(c("Skipping {.obj_type_friendly {obj}}"))
  return()
}

S7::method(list_resources, class_care) <- function(obj) {
  S7::prop(obj, "access") |> list_resources()
}

S7::method(list_resources, care_current) <- function(obj) {
  S7::prop(obj, "resources") |>
    purrr::map(httr2::request) |>
    httr2::req_perform_parallel(on_error = "continue") |>
    httr2::resps_successes() |>
    purrr::map(function(resp)
      parse_string(resp, query = "/data") |>
        tidy_resources()) |>
    yank()
}

S7::method(list_resources, care_temporal) <- function(obj) {
  S7::prop(obj, "resources") |>
    purrr::map(httr2::request) |>
    httr2::req_perform_parallel(on_error = "continue") |>
    httr2::resps_successes() |>
    httr2::resps_data(function(resp)
      parse_string(resp, query = "/data")) |>
    tidy_resources()
}
