# x <- query2(
#   first_name  = starts_with("And"),
#   middle_name = NULL,
#   last_name   = contains("J"),
#   state       = any_of("CA", "GA", "NY"),
#   state_own   = c("GA", "MD"),
#   ccn         = "01256",
#   rate        = between(0.45, 0.67),
#   year        = 2014:2025,
#   or("first_name", "last_name"),
#   and("ccn", "rate")
# )
#
# x |>
#   props("params") |>
#   get_elem("params")
#
# x |>
#   props("groups") |>
#   get_elem("groups")

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
  paste0(
    "filter[<<i>>][condition][memberOf]=",
    x@member_of
  )
}

#' @autoglobal
#' @noRd
place_path <- function(x) {
  paste0(
    "filter[<<i>>][condition][path]=",
    gsub(" ", "+", x, fixed = TRUE)
  )
}

#' @autoglobal
#' @noRd
place_operator <- function(x) {
  paste0(
    "filter[<<i>>][condition][operator]=",
    toupper(
      gsub(" ", "+", x@operator, fixed = TRUE)
    )
  )
}

#' @autoglobal
#' @noRd
place_value <- function(x) {
  if (length(x@value) > 1L) {
    paste0(
      "filter[<<i>>][condition][value][", seq_along(x@value), "]=", x@value)
  } else {
    paste0("filter[<<i>>][condition][value]=", x@value)
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

# Empty Brackets
#' @autoglobal
#' @noRd
query_default_ARG <- function(args) {

  purrr::imap(args, function(x, N) {

    V <- x@value
    O <- tolower(gsub("_", "+", x@operator, fixed = TRUE))
    N <- gsub(" ", "+", N, fixed = TRUE)
    G <- if (empty(x@member_of)) NULL else x@member_of

    c(
      `if`(
        is.null(G),
        NULL,
        paste0("conditions[<<i>>][memberOf]=", G)),
      paste0("conditions[<<i>>][property]=", N),
      paste0("conditions[<<i>>][operator]=", O),
      `if`(
        length(V) > 1L,
        paste0("conditions[<<i>>][value][]=", V),
        paste0("conditions[<<i>>][value]=", V)))
  }) |>
    unname() |>
    purrr::imap(function(x, idx)
      gsub(x           = x,
           pattern     = "<<i>>",
           replacement = idx - 1,
           fixed       = TRUE)
    ) |>
    purrr::map(paste0, collapse = "&")
}
