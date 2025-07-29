# endpoint <- function(alias) {
#
#   x   <- alias_lookup(alias)
#
#   cls <- as_function(glue("class_{x$catalog}"))
#
#   pnt <- `if`(x$catalog == "care",
#               as_function(glue("{x$catalog}_{x$point}")),
#               as_function(glue("class_{x$point}")))
#
#
#   cls(access = pnt(
#     identifier = ifelse(x$point == "current", x$identifier, x$endpoints),
#     metadata = get_metadata(x),
#     dimensions = get_dimensions(x)
#   ))
# }

#' Parse datetime
#'
#' @param x `<chr>` vector to parse; format: "YYYY-MM-DDTHH:MM:SS"
#'
#' @returns `<chr>` parsed ISOdatetime vector
#'
#' @examplesIf rlang::is_interactive()
#' as_datetime("2024-07-29T20:37:53")
#'
#' @seealso [clock::date_time_parse_RFC_3339()]
#' @autoglobal
#' @noRd
as_datetime <- function(x) {
  ISOdatetime(
    substr(x, 1, 4),
    substr(x, 6, 7),
    substr(x, 9, 10),
    substr(x, 12, 13),
    substr(x, 15, 16),
    substr(x, 18, 19)
  )
}

#' Parse `openFDA` date character vectors
#' @autoglobal
#' @noRd
as_fda_date <- function(i) {
  delist(map(i, function(x)
    paste0(
      substr(x, 1, 4),
      substr(x, 5, 6),
      substr(x, 7, 8),
      collapse = "-"
    )))
}

#' @autoglobal
#' @noRd
convert_epoch <- function(x) {
  as.Date(as.POSIXct.numeric(as.numeric(x) / 1000L, origin = "1970-01-01"))
}

#' @autoglobal
#' @keywords internal
#' @noRd
make_join_col <- function(x, col) {
  map(x[[ensym(col)]], function(x) get_elem(as.list(x), "data")) |>
    flatten_column() |>
    na_if("")
}

#' @autoglobal
#' @noRd
luhn_check <- function(x) {

  i <- c(1L, 3L, 5L, 7L, 9L)
  d <- cheapr_rev(as.integer(strsplit(delist(as.character(x)), "")[[1]][1:9]))

  d[i] <- d[i] * 2L
  d[i] <- ifelse(d[i] > 9L, d[i] - 9L, d[i])

  d <- sum(d) + 24L
  d <- (ceiling(d / 10) * 10) - d

  identical(paste0(substr(x, 1, 9), d), x)
}
