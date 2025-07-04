#' @autoglobal
#' @noRd
just_right <- function(x) {
  format(x, justify = "right")
}

#' @autoglobal
#' @noRd
just_left <- function(x) {
  format(x, justify = "left")
}

#' @autoglobal
#' @noRd
lone_not_null_cli <- function(x) {
  cli::col_yellow(unlist(x, use.names = FALSE))
}

#' @autoglobal
#' @noRd
brackets_cli <- function(x) {
  paste0(
    cli::col_silver("["),
    paste0(
      cli::col_yellow((unlist(x, use.names = FALSE))
    ),
    collapse = cli::col_silver(", ")),
    cli::col_silver("]")
  )
}

#' @autoglobal
#' @noRd
calls_cli <- function(x) {
  map(x[are_calls(x)], function(x)
    cli::col_green(deparse1(x)))
}

#' @autoglobal
#' @noRd
brackets <- function(x) {
  paste0("[", x, "]")
}

#' @autoglobal
#' @noRd
seq_along0 <- function(x) {
  seq_along(x) - 1
  # 0L:(length(x) - 1L)
}

#' @autoglobal
#' @noRd
are_length_one <- function(x) {
  list_lengths(x) == 1L
}

#' @autoglobal
#' @noRd
are_length_two <- function(x) {
  list_lengths(x) > 1L
}

#' @autoglobal
#' @noRd
are_null <- function(x) {
  map_lgl(x, is.null)
}

#' @autoglobal
#' @noRd
are_not_null <- function(x) {
  map_lgl(x, Negate(is.null))
}

#' @autoglobal
#' @noRd
are_calls <- function(x) {
  map_lgl(x, is_call)
}

#' @autoglobal
#' @noRd
are_lone_not_null <- function(x) {
  are_length_one(x) & are_not_null(x)
}

#' @autoglobal
#' @rdname query-modifiers
#' @noRd
is_modifier <- function(x) {
  inherits(x, "modifier")
}

#' @autoglobal
#' @noRd
any_calls <- function(x) {
  any(are_calls(x))
}

#' @autoglobal
#' @noRd
any_length_two <- function(x) {
  any(are_length_two(x))
}

#' @autoglobal
#' @noRd
any_null <- function(x) {
  any(are_null(x))
}

#' @autoglobal
#' @noRd
any_lone_not_null <- function(x) {
  any(are_lone_not_null(x))
}

#' @autoglobal
#' @noRd
query_keywords <- function(type) {

  if (is_missing(type)) type <- "default"

  idx_ <- "<<i>>"

  switch(
    type,
    default  = list(
      VERB      = "conditions",
      FIELD     = "[property]=",
      OPERATOR  = "[operator]=",
      VALUE     = "[value]",
      IDX       = idx_,
      BDX       = brackets(idx_)
    ),
    medicare = list(
      VERB      = "filter",
      FIELD     = "[path]=",
      OPERATOR  = "[operator]=",
      VALUE     = "[value]",
      IDX       = idx_,
      BDX       = brackets(idx_)
    )
  )

}

#' @autoglobal
#' @noRd
flatten_query <- function(x) {
  map(x, \(x) paste0(x, collapse = "&")) |>
    unlist(use.names = FALSE) |>
    paste0(collapse = "&")
}
