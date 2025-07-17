#' List resources
#'
#' @param obj `<class_care>` object
#'
#' @returns A list of available API resources.
#'
#' @examples
#' endpoint("care_enroll_prov") |> list_resources()
#' endpoint("quality_payment") |> list_resources()
#' collection("care_in") |> list_resources()
#'
#' @autoglobal
#' @export
list_resources <- new_generic("list_resources", "obj", function(obj) {
  S7_dispatch()
})

#' @autoglobal
#' @noRd
tidy_resources <- function(x) {
  x |>
    fcompute(
      year     = extract_year(name),
      file     = rm_space(gremove(name, " [0-9]{4}|[0-9]{4} ")),
      size     = as_fs_bytes(fileSize),
      ext      = tolower(path_ext(downloadURL)),
      download = downloadURL
    ) |>
    ffill(year) |>
    roworder(-year, ext, -size) |>
    as_fibble()
}

method(list_resources, class_group) <- function(obj) {
  prop(obj, "members") |>
    map(list_resources)
}

method(list_resources, class_catalog) <- function(obj) {
  cli::cli_alert_warning(
    paste0("{.fn list_resources} needs a {.cls class_care}",
      " object, not {.obj_type_friendly {obj}}."))
  invisible(NULL)
}

method(list_resources, class_endpoint) <- function(obj) {
  prop(obj, "access") |>
    list_resources()
}

method(list_resources, care_current) <- function(obj) {
  prop(obj, "resources") |>
    map(request) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    map(function(resp)
      parse_string(resp, query = "/data") |>
        tidy_resources()) |>
    yank()
}

method(list_resources, care_temporal) <- function(obj) {
  prop(obj, "identifier") |>
    get_elem("resources") |>
    map(request) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    resps_data(function(resp)
      parse_string(resp, query = "/data")) |>
    tidy_resources()
}
