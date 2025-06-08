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
#' @param x `care_endpoint` or `care_temporal` object
#' @returns A list of API resources
#' @examplesIf rlang::is_interactive()
#' care_endpoint("care_enrollees") |> list_resources()
#' care_temporal("quality_payment") |> list_resources()
#' care_group("care_hha") |> list_resources()
#' care_group("care_inpatient") |> list_resources()
#' @autoglobal
#' @export
list_resources <- new_generic("list_resources", "x", function(x) {
  S7_dispatch()
})

method(list_resources, class_endpoint) <- function(x) {
  if (clog_(x) != "care") return(NULL)
  resources_(x) |>
    map(request) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    map(function(resp)
      parse_string(resp, query = "/data") |>
        tidy_resources()) |>
    pluck(1)
}

method(list_resources, class_temporal) <- function(x) {
  if (clog_(x) != "care") return(NULL)
  resources_(x) |>
    map(request) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    resps_data(function(resp)
      parse_string(resp, query = "/data")) |>
    tidy_resources()
}

method(list_resources, class_group) <- function(x) {
  members_(x) |>
    map(list_resources, .progress = TRUE) |>
    name_members_(x)
}
