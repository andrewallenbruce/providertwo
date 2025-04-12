#' @include S7_care.R
NULL

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

#' @noRd
#' @autoglobal
st_extract <- function(string, pattern, perl = TRUE, ...) {
  regmatches(
    x = string,
    m = gregexec(
      pattern = pattern,
      text = string,
      perl = perl,
      ...)
    ) |>
    unlist(use.names = FALSE)
}

method(list_resources, class_character) <- function(x) {
  fload(x, query = "/data") |>
    fcompute(
      year     = as.integer(stri_extract_all_regex(name, "[0-9]{4}")),
      file     = stri_replace_all_regex(name, " [0-9]{4}|[0-9]{4} ", ""),
      size     = roundup(fileSize / 1e6),
      ext      = file_ext(downloadURL),
      download = downloadURL
    ) |>
    f_fill(year)
}

method(list_resources, careMain) <- function(x) {
  prop(x, "resources") |>
    list_resources() |>
    roworder(-year, ext, -size) |>
    as_tbl()
}

method(list_resources, careTemp) <- function(x) {

  prop(x, "endpoints") |>
    _[["resources"]] |>
    map(request) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    resps_data(\(resp) resp_body_string(resp) |>
                 fparse(query = "/data")) |>
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
