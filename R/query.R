#' @autoglobal
#' @noRd
brackets <- function(x) {
  paste0("[", x, "]")
}

#' @autoglobal
#' @noRd
query_keywords <- function(type) {
  if (is_missing(type)) type <- "default"

  idx_ <- "<<i>>"

  switch(
    type,
    default = list(
      VRB = "conditions",
      FLD = "[property]=",
      OPR = "[operator]=",
      VAL = "[value]",
      IDX = idx_,
      BDX = brackets(idx_)
    ),
    medicare = list(
      VRB = "filter",
      FLD = "[path]=",
      OPR = "[operator]=",
      VAL = "[value]",
      IDX = idx_,
      BDX = brackets(idx_)
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

#' Create a Query Object
#'
#' @param ... Query arguments. See details for valid query modifiers.
#' @param .type Query type, `"default"` or `"medicare"`.
#' @returns S7 `<class_query>` object.
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
#' @autoglobal
#' @export
query <- function(..., .type = c("default", "medicare")) {

  .type <- arg_match(.type)

  args <- discard(dots_list(..., .homonyms = "error"), is.null)

  .c(VRB, FLD, OPR, VAL, IDX, BDX) %=% query_keywords(type = .type)

  out <- imap(args, function(x, name) {

    fields    <- paste0(VRB, BDX, FLD, name)
    operators <- paste0(VRB, BDX, OPR, if (is_modifier(x)) x[["operator"]] else "=")
    values    <- if (is_modifier(x)) x[["value"]] else unlist(x, use.names = FALSE)

    if (length(values) > 1)

      values <- paste0(VRB, BDX, VAL, "[", seq_along(values), "]=", values)

    if (length(values) == 1)

      values <- paste0(VRB, BDX, VAL, "=", values)

    c(fields, operators, values)

    }) |>
    unname() |>
    imap(function(x, i) greplace(x, IDX, ifelse(.type == "default", i - 1, i)))

  i <- discard(enexprs(...), is.null)

  if (any_calls(i)) i[are_calls(i)] <- deparse_calls(i)

  class_query(input = i, output = out)
}
