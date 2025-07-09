#' @autoglobal
#' @noRd
brackets <- function(x) {
  paste0("[", x, "]")
}

#' @autoglobal
#' @noRd
flatten_query <- function(x) {
  map(x, \(x) paste0(x, collapse = "&")) |>
    unlist(use.names = FALSE) |>
    paste0(collapse = "&")
}

#' @autoglobal
#' @noRd
query_keywords <- function(type) {

  if (is_missing(type)) type <- "default"

  list_combine(
    switch(
      type,
      default  = list(VRB = "conditions", FLD = "[property]="),
      medicare = list(VRB = "filter",     FLD = "[path]=")
    ),
    OPR = "[operator]=",
    VAL = "[value]",
    IDX = "<<i>>",
    BDX = brackets("<<i>>")
  )
}

#' Create a Query Object
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Named conditions where the names are API fields.
#'
#' @param .type Query type, `"default"` or `"medicare"`.
#'
#' @returns S7 `<class_query>` object.
#'
#' @examples
#' query(
#'   first_name = starts_with_("Andr"),
#'   last_name  = contains_("J"),
#'   state      = in_(c("CA", "GA", "NY")),
#'   city       = equals_(c("Atlanta", "Los Angeles"), negate = TRUE),
#'   state_own  = c("GA", "MD"),
#'   npi        = npi_ex$k,
#'   ccn        = "01256",
#'   rate       = between_(0.45, 0.67))
#'
#' query(
#'   first_name = starts_with_("Andr"),
#'   last_name  = contains_("J"),
#'   state      = in_(c("CA", "GA", "NY")),
#'   city       = equals_(c("Atlanta", "Los Angeles"), negate = TRUE),
#'   state_own  = c("GA", "MD"),
#'   npi        = npi_ex$k,
#'   ccn        = "01256",
#'   .type      = "medicare")
#'
#' @autoglobal
#' @export
query <- function(..., .type = c("default", "medicare")) {
  .type <- arg_match(.type)

  args <- discard(dots_list(..., .homonyms = "error"), is.null)

  k <- query_keywords(type = .type)
  v <- paste0(k$VRB, k$BDX, if (.type == "medicare") "[condition]" else NULL)

  o <- imap(args, function(x, name) {
    val <- if (is_modifier(x))
      x[["value"]]
    else
      unlist(x, use.names = FALSE)

    c(
      paste0(v, k$FLD, name),
      paste0(v, k$OPR, ifelse(is_modifier(x), x[["operator"]], "=")),
      if (length(val) > 1)
        paste0(v, k$VAL, "[", seq_along(val), "]=", val)
      else
        paste0(v, k$VAL, "=", val)
    )
  }) |>
    unname() |>
    imap(function(x, idx)
      greplace(x, k$IDX, idx - 1))

  i <- discard(enexprs(...), is.null)
  # if (any_calls(i)) i[are_calls(i)] <- deparse_calls(i)
  class_query(input = i, output = o)
}
