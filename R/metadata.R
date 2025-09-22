#' @noRd
#' @autoglobal
get_meta <- function(x) {
  purrr::compact(
    list(
      alias       = x$alias,
      title       = x$title,
      description = x$description,
      modified    = x$modified,
      group       = x$group,
      issued      = x$issued,
      released    = x$released,
      temporal    = x$temporal,
      periodicity = x$periodicity,
      download    = x$download,
      resources   = x$resources,
      dictionary  = x$dictionary,
      site        = x$site,
      references  = x$references
    )
  )
}

#' @noRd
#' @autoglobal
get_meta2 <- function(x) {
  class_metadata(
    alias       = x$alias     %||% character(0L),
    title       = x$title     %||% character(0L),
    modified    = x$modified  %||% numeric(0L),
    resources   = x$resources %||% character(0L)
  )
}
