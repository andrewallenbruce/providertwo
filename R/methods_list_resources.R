#' @include S7_care.R
NULL

#' @autoglobal
#' @noRd
tidy_resources <- function(x) {
  x |>
    fcompute(
      year     = extract_year(name),
      file     = greplace(stri_replace_all_regex(name, " [0-9]{4}|[0-9]{4} ", ""), "  ", " "),
      size     = as_fs_bytes(fileSize),
      ext      = tolower(path_ext(downloadURL)),
      download = downloadURL
    ) |>
    f_fill(year) |>
    roworder(-year, ext, -size) |>
    as_tbl()
}

#' @name list_resources
#' @title List resources
#' @param x `care_endpoint`, `care_group`, `care_temporal`, or `care_troup` object
#' @returns A list of API resources
#' @examples
#' care_endpoint("care_enrollees") |> list_resources()
#' care_group("care_hha") |> list_resources()
#' care_temporal("quality_payment") |> list_resources()
#' care_troup("care_inpatient") |> list_resources()
#' @autoglobal
#' @export
list_resources <- new_generic("list_resources", "x", function(x) {
  S7_dispatch()
})

method(list_resources, care_endpoint) <- function(x) {
  x@metadata$resources |>
    as.list() |>
    map(request) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    map(\(resp) resp_body_string(resp) |>
          fparse(query = "/data") |>
          tidy_resources()) |>
    _[[1]]
}

method(list_resources, care_temporal) <- function(x) {
  x@endpoints$resources |>
    map(request) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    resps_data(
      \(resp)
      resp_body_string(resp) |>
        fparse(query = "/data")) |>
    tidy_resources()
}

method(list_resources, class_group) <- function(x) {
  members(x) |>
    map(list_resources, .progress = TRUE) |>
    name_members(x)
}
