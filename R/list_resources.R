#' List resources
#' @param x `class_endpoint` or `class_temporal` object
#' @returns A list of available API resources, either for download or to browse online.
#' @examplesIf rlang::is_interactive()
#' new_endpoint("care_enroll_prov") |> list_resources()
#' new_endpoint("quality_payment") |> list_resources()
#' new_collection("care_hha") |> list_resources()
#' new_collection("care_in") |> list_resources()
#' @autoglobal
#' @export
list_resources <- new_generic("list_resources", "x", function(x) {
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

method(list_resources, class_group) <- function(x) {
  prop(x, "members") |>
    map(list_resources)
}

method(list_resources, class_catalog) <- function(x) {
  cli::cli_alert_warning(
    "{.fn list_resources} requires {.obj_type_friendly {class_care()}}, not {.obj_type_friendly {x}}.",
    wrap = TRUE)
  invisible(NULL)
}

method(list_resources, class_care) <- function(x) {
  prop(x, "resources") |>
    list_resources()
}

method(list_resources, class_endpoint) <- function(x) {
  prop(x, "url") |>
    map(request) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    map(function(resp)
      parse_string(resp, query = "/data") |>
        tidy_resources()) |>
    pluck(1L)
}

method(list_resources, class_temporal) <- function(x) {
  prop(x, "url") |>
    get_elem("resources") |>
    map(request) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    resps_data(function(resp)
      parse_string(resp, query = "/data")) |>
    tidy_resources()
}
