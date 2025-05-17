#' @include S7_care.R
NULL

#' @autoglobal
#' @noRd
get_resources <- function(obj) {
  check_is_S7(obj)
  switch(
    class(obj)[1],
    care_endpoint = as.list(prop(obj, "metadata")$resources),
    care_temporal = as.list(prop(obj, "endpoints")$resources),
  )
}

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
#' @param x `care_endpoint`, `care_group`, `care_temporal`
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
  get_resources(x) |>
    map(request) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    map(function(resp)
      care_parse(resp) |>
        tidy_resources()) |>
    pluck(1)
}

method(list_resources, care_temporal) <- function(x) {
  get_resources(x) |>
    map(request) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    resps_data(function(resp)
      care_parse(resp)) |>
    tidy_resources()
}

method(list_resources, class_group) <- function(x) {
  all(members(x) |>
        map_lgl(function(x)
          S7_inherits(x, care_endpoint) |
          S7_inherits(x, care_temporal)))

  members(x) |>
    map(list_resources, .progress = TRUE) |>
    name_members(x)
}
