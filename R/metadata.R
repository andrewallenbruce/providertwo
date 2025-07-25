#' @noRd
#' @autoglobal
null_if <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.na(x)) NULL else x
}

#' @noRd
#' @autoglobal
unlist_if <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.na(x)) return(NULL)
  if (is.list(x)) unlist(x, use.names = FALSE) else x
}

#' @noRd
#' @autoglobal
get_metadata <- function(x) {
  compact(
    list(
      title       = x$title,
      description = x$description,
      modified    = x$modified,
      group       = x$group,
      issued      = x$issued,
      released    = x$released,
      temporal    = x$temporal,
      periodicity = x$periodicity,
      download    = x$download,
      resources   = unlist_if(x$resources),
      dictionary  = x$dictionary,
      site        = x$site,
      references  = x$references
    )
  )
}
