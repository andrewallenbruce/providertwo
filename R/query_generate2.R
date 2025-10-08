# 2 Group Conjunctions
# Index ([2]) ids the group
#
# "filter[g1][group][conjunction]=OR"
# "filter[g2][group][conjunction]=AND"
#
# Add memberOf to params in group
#
# "filter[1][condition][memberOf]=g2"

#' @autoglobal
#' @noRd
place_member_of <- function(x) {
  if (empty(x@member_of)) {
    return(NULL)
  }
  paste0("filter[<<i>>][condition][memberOf]=", x@member_of)
}

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
place_member_of2 <- function(x) {
  if (empty(x@member_of)) {
    return(NULL)
  }
  paste0("conditions[<<i>>][memberOf]=", x@member_of)
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
      c(place_member_of(x),
        place_path(N),
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
query_care_GRP <- function(args) {
  args$group |>
    purrr::imap(function(x, N)
      paste0("filter[", N, "][group][conjunction]=", x)) |>
    unname() |>
    purrr::map(paste0, collapse = "&")
}

#' @autoglobal
#' @noRd
query_def_ARG <- function(args) {
  args[rlang::names2(args) %!=% "group"] |>
    purrr::imap(function(x, N) {
      c(place_member_of2(x),
        place_path2(N),
        place_operator2(x),
        place_value2(x))
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
query_def_GRP <- function(args) {
  args$group |>
    purrr::imap(function(x, N)
      paste0("conditions[", N, "][conjunction]=", x)) |>
    unname() |>
    purrr::map(paste0, collapse = "&")
}
