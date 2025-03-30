#' @name list_resources
#' @title List resources
#'
#' @param x An object of class `MainCurrent` or `class_character`
#' @param ... Additional arguments?
#'
#' @returns A list of API resources
#' @examples
#' MainCurrent("enrollees") |> list_resources()
#' MainTemporal("quality_payment") |> list_resources()
#'
#' @autoglobal
#' @rdname Main
#' @export
list_resources <- new_generic("list_resources", "x")

method(list_resources, MainCurrent) <- function(x) {
  fload(x@resources, query = "/data") |>
    fcompute(
      file     = name,
      size     = roundup(fileSize / 1e6),
      ext      = file_ext(downloadURL),
      download = downloadURL
    ) |>
    roworder(ext, -size) |>
    as_tbl()
}

method(list_resources, MainTemporal) <- function(x) {
  map(x@endpoints$resources, function(x)
    fload(x, query = "/data") |>
      fcompute(
        year     = as_int(stri_extract_all_regex(name, "[0-9]{4}")),
        file     = stri_replace_all_regex(name, " [0-9]{4}", ""),
        size     = roundup(fileSize / 1e6),
        ext      = file_ext(downloadURL),
        download = downloadURL
      ) |>
      roworder(ext, -size)) |>
    rowbind() |>
    f_fill(year) |>
    as_tbl()
}

method(list_resources, class_character) <- function(x) {
  fload(x, query = "/data") |>
    fcompute(
      file     = name,
      size     = roundup(fileSize / 1e6),
      ext      = file_ext(downloadURL),
      download = downloadURL
    ) |>
    roworder(ext, -size)
}



