# q2 <- list(
#   `conditions[0][property]` = "state",
#   `conditions[0][operator]` = "IN",
#   `conditions[0][value][1]` = "CA",
#   `conditions[0][value][2]` = "GA",
#   `conditions[0][value][3]` = "NY")
#
# x <- list(
# first_name ~ starts_with_("Andr"),
# last_name ~ contains_("J"),
# state = ~ in_(c("CA", "GA", "NY")),
# country = in_(c("CA", "GA", "NY")),
# state_owner = c("GA", "MD"),
# npi = npi_ex$k,
# npi_owner = npi_ex$k[1],
# ccn = "01256",
# pac = NULL)
#
# x
#
# is_unnamed_formula(x)
# is_named_formula(x)
# !is_unnamed_formula(x) & !is_named_formula(x) & is_length_one(x)
# !is_unnamed_formula(x) & !is_named_formula(x) & is_length_two(x)
#
# check_names_unique(x)
# check_unnamed_formulas(x)

#' @autoglobal
#' @noRd
query_cli <- function(x) {

  if (any(is_named_rhs(x))) {

    x[is_named_rhs(x)] <- map(
      x[is_named_rhs(x)], function(x) {
        cli::col_green(deparse1(f_rhs(x)))
        })

  }

  if (any(is_len_two(x))) {

    x[is_len_two(x)] <- brackets(
      cli::col_yellow(
        unlist(
          x[is_len_two(x)],
          use.names = FALSE)
        )
      )
  }

  if (any(is_nil(x))) x[is_nil(x)] <- cli::col_red("NULL")

  if (any(is_len_one_not_nil(x))) {

    x[is_len_one_not_nil(x)] <- cli::col_yellow(
      unlist(
        x[is_len_one_not_nil(x)],
        use.names = FALSE)
      )
  }

  VALUE <- fmt_left(unlist(x, use.names = FALSE))

  FIELD <- fmt_right(names(x))

  EQUAL <- cli::col_black("=")

  glue("{FIELD} {EQUAL} {VALUE}")
}

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
#   # first_name ~ starts_with_("Andr"),
#   # last_name ~ contains_("J"),
#   state = ~ in_(c("CA", "GA", "NY")),
#   # country = in_(c("CA", "GA", "NY")),
#   # state_owner = c("GA", "MD"),
#   # npi = npi_ex$k,
#   # npi_owner = npi_ex$k[1],
#   # ccn = "01256",
#   # pac = NULL
# )
# x
# args = list(
#   state = c("GA", "MD"),
#   last_name = "SMITH",
#   country = ~ in_(c("CA", "GA", "NY")),
#   npi = 1234567890,
#   PECOS = NULL
# )
# args <- convert_named_formula(args)
# generate_query(args)
#' @autoglobal
#' @noRd
generate_query <- function(args, type = "default") {

  .c(VERB, FIELD, OPERATOR, VALUE, IDX) %=% query_keywords(type)

  discard(args, is.null) |>
    imap(
      function(x, name) {

    p <- paste0(VERB, IDX, FIELD, name)
    o <- paste0(VERB, IDX, OPERATOR, "=")
    v <- unlist(x, use.names = FALSE)

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
#' @examples
#' args = list(state = c("GA", "MD"),
#'             last_name = "SMITH",
#'             npi = 1234567890,
#'             PECOS = NULL)
#'
#' query_formatter(args)
#'
#' @param args named `<list>` of `<chr>` arguments
#' @returns `<list>` of formatted query `<exprs>`
#' @autoglobal
#' @rdname query-format
#' @export
query_formatter <- function(args) {

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
