

#' @autoglobal
#' @noRd
place_path <- function(x) {
  paste0("filter[<<i>>][condition][path]=", gsub(" ", "+", x, fixed = TRUE))
}

#' @autoglobal
#' @noRd
place_operator <- function(x) {
  paste0("filter[<<i>>][condition][operator]=", toupper(gsub(" ", "+", x@operator, fixed = TRUE)))
}

#' @autoglobal
#' @noRd
place_value <- function(x) {
  if (length(x@value) > 1L) {
    paste0("filter[<<i>>][condition][value][", seq_along(x@value), "]=", x@value)
  } else {
    paste0("filter[<<i>>][condition][value]=", x@value)
  }
}

#' @autoglobal
#' @noRd
place_path2 <- function(x) {
  paste0("conditions[<<i>>][property]=", gsub(" ", "+", x, fixed = TRUE))
}

#' @autoglobal
#' @noRd
place_operator2 <- function(x) {
  paste0("conditions[<<i>>][operator]=", tolower(gsub("_", "+", x@operator, fixed = TRUE)))
}

#' @autoglobal
#' @noRd
place_value2 <- function(x) {
  if (length(x@value) > 1L) {
    paste0("conditions[<<i>>][value][]=", x@value)
  } else {
    paste0("conditions[<<i>>][value]=", x@value)
  }
}

#' @autoglobal
#' @noRd
place_value2_1 <- function(x) {
  if (length(x@value) > 1L) {
    paste0("conditions[<<i>>][value][", seq_along(x@value), "]=", x@value)
  } else {
    paste0("conditions[<<i>>][value]=", x@value)
  }
}

#' @autoglobal
#' @noRd
query_care_ARG <- function(args) {
  args[rlang::names2(args) %!=% "group"] |>
    purrr::imap(function(x, N) {
      c(place_path(N),
        place_operator(x),
        place_value(x))
    }) |>
    unname() |>
    purrr::imap(function(x, idx)
      gsub(
        x           = x,
        pattern     = "<<i>>",
        replacement = idx,
        fixed       = TRUE
      )) |>
    purrr::map(paste0, collapse = "&")
}

#' @autoglobal
#' @noRd
query_def_ARG <- function(args) {
  args[rlang::names2(args) %!=% "group"] |>
    purrr::imap(function(x, N) {
      c(place_path2(N),
        place_operator2(x),
        place_value2(x))
    }) |>
    unname() |>
    purrr::imap(function(x, idx)
      gsub(
        x           = x,
        pattern     = "<<i>>",
        replacement = idx - 1L,
        fixed       = TRUE
      )) |>
    purrr::map(paste0, collapse = "&")
}
