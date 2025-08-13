#' List resources
#'
#' @param obj `<class_care>` object
#'
#' @returns A list of available API resources.
#'
#' @examples
#' roll <- endpoint("enroll_prov")
#' qppe <- endpoint("qppe")
#' hosp <- collection("in_hosp")
#' rand <- group("asc_facility", "enterprise", "lab_fee")
#' list_resources(roll)
#' list(qppe, hosp, rand) |> purrr::map(list_resources)
#' @autoglobal
#' @export
list_resources <- new_generic("list_resources", "obj", function(obj) {
  S7_dispatch()
})

#' @autoglobal
#' @noRd
tidy_resources <- function(x) {
  x |>
    collapse::mtt(
      year     = extract_year(name),
      year     = ifelse(is.na(year), extract_year(downloadURL), year),
      file     = rm_space(gremove(name, " [0-9]{4}|[0-9]{4} ")),
      size     = fs::as_fs_bytes(fileSize),
      ext      = tolower(fs::path_ext(downloadURL)),
      download = downloadURL,
      .keep = c("year", "file", "size", "ext")) |>
    ffill(year) |>
    roworder(-year, ext, -size) |>
    as_fibble()
}

method(list_resources, class_group) <- function(obj) {
  prop(obj, "members") |> map(list_resources)
}

method(list_resources, class_catalog) <- function(obj) {
  cli::cli_alert_warning(c("Skipping {.obj_type_friendly {obj}}"))
  return()
}

method(list_resources, class_care) <- function(obj) {
  prop(obj, "access") |> list_resources()
}

method(list_resources, care_current) <- function(obj) {
  cli::cli_inform(c("{.pkg {cli::symbol$menu}} {.field {meta(obj)$title}}"))
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

method(list_resources, care_temporal) <- function(obj) {
  cli::cli_inform(c("{.pkg {cli::symbol$menu}} {.field {meta(obj)$title}}"))
  prop(obj, "identifier") |>
    get_elem("resources") |>
    map(request) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    resps_data(function(resp) parse_string(resp, query = "/data")) |>
    tidy_resources()
}
