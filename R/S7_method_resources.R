#' @include S7_care.R
NULL

#' @autoglobal
#' @noRd
tidy_resources <- function(x) {
  x |>
    fcompute(
      year     = as.integer(stri_extract_first_regex(name, "[12]{1}[0-9]{3}")),
      file     = gsub("  ", " ", stri_replace_all_regex(name, " [0-9]{4}|[0-9]{4} ", ""), perl = TRUE),
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
#' care_endpoint("enrollees") |> list_resources()
#' care_group("HHA") |> list_resources()
#' care_temporal("quality_payment") |> list_resources()
#' care_troup("inpatient") |> list_resources()
#' @autoglobal
#' @export
list_resources <- new_generic("list_resources", "x", function(x) {
  S7_dispatch()
})

method(list_resources, care_endpoint) <- function(x) {
  x@metadata$resources |>
    request() |>
    req_perform() |>
    resp_body_string() |>
    fparse(query = "/data") |>
    tidy_resources()
}

method(list_resources, care_group) <- function(x) {
  x@members |>
    map(\(x)
        x@metadata$resources |>
          request()
        ) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    map(\(resp)
        resp_body_string(resp) |>
          fparse(query = "/data") |>
          tidy_resources()
        ) |>
    set_member_names(x@members)
}

method(list_resources, care_temporal) <- function(x) {
  x@endpoints$resources |>
    map(request) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    resps_data(
      \(resp)
      resp_body_string(resp) |>
        fparse(query = "/data")
      ) |>
    tidy_resources()
}

method(list_resources, care_troup) <- function(x) {
  x@members |>
    map(\(x)
        x@endpoints$resources |>
          map(request) |>
      req_perform_parallel(on_error = "continue") |>
      resps_successes() |>
      resps_data(
        \(resp)
        resp_body_string(resp) |>
          fparse(query = "/data") |>
          tidy_resources()
      )
    ) |>
    set_member_names(x@members)
}
