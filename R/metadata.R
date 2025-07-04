#' @noRd
#' @autoglobal
null_if <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.na(x)) NULL else x
}

#' @noRd
#' @autoglobal
unlist_if <- function(x) {
  if (is.list(x)) unlist(x, use.names = FALSE) else x
}

# get_metadata(list(title = "metadata_"))
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

#' @autoglobal
#' @noRd
get_identifier <- function(x) {
  switch(
    x$pnt,
    endpoint = class_current(x$identifier),
    temporal = switch(
      x$clg,
      care = class_temporal(slt(x$endpoints, -resources)),
      class_temporal(x$endpoints))
  )
}

#' @autoglobal
#' @noRd
get_resources <- function(x) {
  switch(
    x$pnt,
    endpoint = class_current(x$resources),
    temporal = class_temporal(slt(x$endpoints, year, resources)))
}
