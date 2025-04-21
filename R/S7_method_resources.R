#' @include S7_care.R
NULL

#' @autoglobal
#' @noRd
.tidy_resources <- \(x) {
  x |>
    fcompute(
      year     = as.integer(stri_extract_all_regex(name, "[0-9]{4}")),
      file     = stri_replace_all_regex(name, " [0-9]{4}|[0-9]{4} ", ""),
      size     = roundup(fileSize / 1e6),
      ext      = file_ext(downloadURL),
      download = downloadURL
    ) |>
    f_fill(year) |>
    roworder(-year, ext, -size) |>
    as_tbl()
}

#' @name list_resources
#' @title List resources
#'
#' @param x Object of class `careMain` or `careTemp`
#'
#' @returns A list of API resources
#'
#' @examples
#' careMain("enrollees") |> list_resources()
#' careTemp("quality_payment") |> list_resources()
#' @autoglobal
#' @export
list_resources <- new_generic("list_resources", "x", function(x) {
  S7_dispatch()
})

method(list_resources, class_character) <- function(x) {
  fload(x, query = "/data") |>
    .tidy_resources()
}

method(list_resources, careMain) <- function(x) {
  prop(x, "resources") |>
    list_resources()
}

method(list_resources, careTemp) <- function(x) {

  prop(x, "endpoints") |>
    _[["resources"]] |>
    map(request) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    resps_data(\(resp) resp_body_string(resp) |>
                 fparse(query = "/data")) |>
    .tidy_resources()
}
