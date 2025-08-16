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
    ffill(year) |>
    as_fibble() |>
    collapse::mtt(
      year     = ifelse(is.na(year), extract_year(download), year))
}

#' List resources
#'
#' @param obj `<class_care>` object
#'
#' @returns A list of available API resources.
#'
#' @examples
#' list_resources(endpoint("enroll_prov"))
#' list_resources(endpoint("qppe"))
#' list_resources(collection("in_hosp"))
#' list_resources(group("asc_facility", "enterprise", "lab_fee"))
#' @autoglobal
#' @export
list_resources <- S7::new_generic("list_resources", "obj", function(obj) {
  S7::S7_dispatch()
})

S7::method(list_resources, class_group) <- function(obj) {
  S7::prop(obj, "members") |> map(list_resources)
}

S7::method(list_resources, class_catalog) <- function(obj) {
  cli::cli_alert_warning(c("Skipping {.obj_type_friendly {obj}}"))
  return()
}

S7::method(list_resources, class_care) <- function(obj) {
  S7::prop(obj, "access") |> list_resources()
}

S7::method(list_resources, care_current) <- function(obj) {
  meta(obj) |>
    get_elem("resources") |>
    map(request) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    map(function(resp)
      parse_string(resp, query = "/data") |>
        tidy_resources()) |>
    yank()
}

S7::method(list_resources, care_temporal) <- function(obj) {
    meta(obj) |>
    get_elem("resources") |>
    map(request) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    resps_data(function(resp) parse_string(resp, query = "/data")) |>
    tidy_resources()
}
