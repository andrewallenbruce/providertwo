#' @name list_resources
#' @title List resources from a `MainCurrent` or `class_character` object
#' @param x An object of class `MainCurrent` or `class_character`
#' @param ... Additional arguments?
#' @returns A list of API resources
#' @examples
#' MainCurrent("enrollees") |> list_resources()
#'
#' @autoglobal
#' @export
list_resources <- new_generic("list_resources", "x")

method(list_resources, MainCurrent) <- function(x) {
  fload(x@resources, query = "/data") |>
    fcompute(
      file = name,
      size = roundup(fileSize / 1e6),
      ext = file_ext(downloadURL),
      download = downloadURL
    ) |>
    roworder(ext, -size)
}

method(list_resources, class_character) <- function(x) {
  fload(x, query = "/data") |>
    fcompute(
      file = name,
      size = roundup(fileSize / 1e6),
      ext = file_ext(downloadURL),
      download = downloadURL
    ) |>
    roworder(ext, -size)
}
