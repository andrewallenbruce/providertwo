#' Create a Query Object
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Named conditions where the names are API fields.
#'
#' @returns S7 `<class_query>` object.
#'
#' @examples
#' new_query(
#'   first_name  = starts_with("And"),
#'   middle_name = NULL,
#'   last_name   = contains("J"),
#'   state       = any_of(c("CA", "GA", "NY")),
#'   city        = all_but(c("Atlanta", "Los Angeles")),
#'   state_own   = c("GA", "MD"),
#'   npi         = npi_ex$k,
#'   ccn         = "01256",
#'   rate        = between(0.45, 0.67),
#'   year        = 2014:2025)
#' @autoglobal
#' @export
new_query <- function(...) {
  class_query(
    input  = purrr::compact(rlang::enexprs(...)),
    params = purrr::compact(rlang::dots_list(..., .homonyms = "error"))
  )
}

#' @autoglobal
#' @noRd
query_care <- function(args) {

  purrr::imap(args, function(x, N) {

    V <- if (is_modifier(x)) x@value else unlist(x, use.names = FALSE)
    O <- if (is_modifier(x)) toupper(x@operator) else "="
    N <- gsub(" ", "+", N, fixed = TRUE)

    c(paste0("filter[<<i>>][condition][path]=", N),
      paste0("filter[<<i>>][condition][operator]=", O),
      `if`(
        length(V) > 1L,
        paste0("filter[<<i>>][condition][value][", seq_along(V), "]=", V),
        paste0("filter[<<i>>][condition][value]=", V)))
  }) |>
    unname() |>
    purrr::imap(function(x, idx)
      gsub(x           = x,
           pattern     = "<<i>>",
           replacement = idx,
           fixed       = TRUE)
      ) |>
    purrr::map(paste0, collapse = "&")
}

#' @autoglobal
#' @noRd
query_default <- function(args) {

  purrr::imap(args, function(x, N) {

    V <- if (is_modifier(x)) x@value else unlist(x, use.names = FALSE)
    O <- if (is_modifier(x)) tolower(gsub("_", "+", x@operator, fixed = TRUE)) else "="
    N <- gsub(" ", "+", N, fixed = TRUE)

    c(paste0("conditions[<<i>>][property]=", N),
      paste0("conditions[<<i>>][operator]=", O),
      `if`(
        length(V) > 1L,
        paste0("conditions[<<i>>][value][", seq_along(V), "]=", V),
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

#' @autoglobal
#' @noRd
query_match <- function(obj, qry) {
  params <- qry@params[names(qry@params) %!in_% "year"]
  fields <- obj@fields@keys

  # TODO Use a modifier on "year" parameter if meant for an API field?
  # Remove "year" if it exists, to be applied to temporal endpoints
  field_clean <- clean_names(fields)

  x <- rlang::set_names(
    params[collapse::fmatch(field_clean,
                            names(params),
                            nomatch = 0L,
                            overid = 2)],
    fields[sort(collapse::fmatch(names(params),
                                 field_clean,
                                 nomatch = 0L,
                                 overid = 2))]
    )

  # TODO Move this to each method
  if (rlang::is_empty(x)) {

    cli::cli_alert_warning(
      c("{.field {obj@metadata$title}}: ",
        paste0(
          "{.pkg {length(names(params))} params} ",
          "{cli::col_red(cli::symbol$neq)} ",
          "{.pkg {length(fields)} fields}"
          )
        )
      )
    return(invisible(NULL))
  }
  x
}
