# q2 <- list(
#   `conditions[0][property]` = "state",
#   `conditions[0][operator]` = "IN",
#   `conditions[0][value][1]` = "CA",
#   `conditions[0][value][2]` = "GA",
#   `conditions[0][value][3]` = "NY")
#
# encodeString(x, quote = '"')
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
check_names_unique <- function(x, call = caller_env()) {

  x <- names(x[have_name(x)])

  if (anyDuplicated(x)) {

    i <- which_(fduplicated(x))

    cli_abort(c(
      "x" = "Field{?s} {.field {x[i]}} appea{?rs/rs/r} multiple times."
      ),
      call = call)
  }
}

#' @autoglobal
#' @noRd
check_unnamed_formulas <- function(x, call = caller_env()) {

  if (any(is_unnamed_full_formula(x))) {

    x <- map(x[is_unnamed_full_formula(x)], \(x) as_label(x)) |>
      unlist(use.names = FALSE) |>
      set_names("*")

    cli_abort(c(
      "x" = "{.emph Unnamed formulas} detected:",
      cli::col_yellow(cli::format_bullets_raw(x))),
      call = call)
  }
}

#' @autoglobal
#' @noRd
convert_named_formula <- function(x) {

  if (any(is_named_rhs_formula(x))) {

    i <- is_named_rhs_formula(x)

    tmp <- x[i]

    rhs <- map(tmp, f_rhs)

    x[i] <- rhs
  }
  x
}

#' @autoglobal
#' @noRd
query_cli <- function(x) {

  if (any(is_length_two(x))) {

    two <- x[is_length_two(x)]

    x[is_length_two(x)] <- paste0("[", paste0(unlist(two, use.names = FALSE), collapse = ", "), "]")

    # names(x[is_length_two(x)]) <- names(two)

  }

  if (any(map_lgl(x, is.null))) x[map_lgl(x, is.null)] <- cli::col_red("NULL")

  VALUE <- format(unlist(x, use.names = FALSE), justify = "left")

  FIELD <- format(names(x), justify = "right")

  glue("{FIELD} = {VALUE}")
}

#' @autoglobal
#' @noRd
generate_query <- function(args, type = "def") {

  args <- discard(args, is.null)

  key <- switch(
    match.arg(type, c("def", "care")),
    care = list(VERB = "filter",     FIELD = "path"),
    def  = list(VERB = "conditions", FIELD = "property"))

  imap(args, function(x, m) {

    p <- paste0(key$VERB, "[<<i>>][", key$FIELD, "]=", m, "&")
    o <- paste0(key$VERB, "[<<i>>][operator]=", "=", "&")
    v <- unlist(x, use.names = FALSE)

    if (length(v) > 1)

      v <- paste0(key$VERB, "[<<i>>][value][", seq_along(v), "]=", v, "&")

    if (length(v) == 1)

      v <- paste0(key$VERB, "[<<i>>][value]=", v, "&")

    c(p, o, v)

  }) |>
    unname() |>
    imap(function(x, idx) {

      greplace(x, "<<i>>", if (type == "def") idx - 1 else idx) |>
        paste0(collapse = "")

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

  args <- discard(args, is.null)

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
