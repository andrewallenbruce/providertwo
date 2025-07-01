# x <- list(
#   first_name ~ starts_with_("Andr"),  #! Unnamed Formula
#   state = ~ in_(c("CA", "GA", "NY")), # CORRECT
#   country = in_(c("CA", "GA", "NY")), #?? Named but not a formula, renders correctly
#   state_owner = c("GA", "MD"),        # CORRECT
#   npi = npi_ex$k,
#   npi = npi_ex$k[1],                  #! Name already used
#   pac = NULL                          # NULLs are discarded
# )
#
# generate_query(x)

# Do I even need to use the formula syntax?
# Look at the country argument above.

#' @autoglobal
#' @noRd
query_keywords <- function(type) {
  type <- match.arg(type, c("default", "medicare"))

  switch(
    type,
    default  = list(VERB      = "conditions",
                    FIELD     = "[property]=",
                    OPERATOR  = "[operator]=",
                    VALUE     = "[value]",
                    IDX       = "[<<i>>]"),
    medicare = list(VERB      = "filter",
                    FIELD     = "[path]=",
                    OPERATOR  = "[operator]=",
                    VALUE     = "[value]",
                    IDX       = "[<<i>>]")
  )

}

# x <- list(
#   first_name ~ starts_with_("Andr"),
#   last_name ~ contains_("J"),
#   state = ~ in_(c("CA", "GA", "NY")),
#   country = in_(c("CA", "GA", "NY")),
#   state_owner = c("GA", "MD"),
#   npi = npi_ex$k,
#   npi_owner = npi_ex$k[1],
#   ccn = "01256",
#   pac = NULL
# )
# generate_query(x)
# generate_query(x, type = "medicare")
#
# x = list(
#   state = c("GA", "MD"),
#   last_name = "SMITH",
#   country = ~ in_(c("CA", "GA", "NY")),
#   npi = 1234567890,
#   PECOS = NULL
# )
#
# generate_query(x)
# generate_query(x, type = "medicare")
#' @autoglobal
#' @noRd
generate_query <- function(args, type = "default") {

  check_unnamed_formulas(args)
  check_names_unique(args)

  .c(VERB, FIELD, OPERATOR, VALUE, IDX) %=% query_keywords(type)

  discard(args, is.null) |>
    convert_named_rhs() |>
    imap(
      function(x, name) {

    p <- paste0(VERB, IDX, FIELD, name)
    o <- paste0(VERB, IDX, OPERATOR, if (is_modifier(x)) x[["operator"]] else "=")
    v <- if (is_modifier(x)) x[["value"]] else unlist(x, use.names = FALSE)

    if (length(v) > 1)

      v <- paste0(VERB, IDX, VALUE, "[", seq_along(v), "]=", v)

    if (length(v) == 1)

      v <- paste0(VERB, IDX, VALUE, "=", v)

    c(p, o, v)

  }) |>
    unname() |>
    imap(
      function(x, idx) {

      greplace(x, "<<i>>", if (type == "default") idx - 1 else idx)

    })
}

#' Format Queries
#'
#' @examples
#' x = list(state = c("GA", "MD"),
#'             last_name = "SMITH",
#'             npi = 1234567890,
#'             PECOS = NULL)
#'
#' new_query(x)
#'
#' @param args named `<list>` of `<chr>` arguments
#' @returns `<list>` of formatted query `<exprs>`
#' @autoglobal
#' @rdname query-format
#' @export
new_query <- function(args) {

  cat(query_cli(args), sep = "\n")
  cat("\n")

  qry <- generate_query(args)

  map(qry, function(x) {
    strsplit(x, "&") |>
      unlist() |>
      append("\n")
  }) |>
    unlist() |>
    cat(sep = "\n")

  invisible(qry)

}
