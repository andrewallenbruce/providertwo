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
# generate_query(x, .type = "medicare")
#' @autoglobal
#' @noRd
generate_query <- function(args, .type) {

  check_unnamed_formulas(args)
  check_names_unique(args)

  .c(VERB, FIELD, OPERATOR, VALUE, IDX, BDX) %=% query_keywords(type = .type)

  discard(args, is.null) |>
    convert_named_rhs() |>
    imap(
      function(x, name) {

    p <- paste0(VERB, BDX, FIELD, name)
    o <- paste0(VERB, BDX, OPERATOR, if (is_modifier(x)) x[["operator"]] else "=")
    v <- if (is_modifier(x)) x[["value"]] else unlist(x, use.names = FALSE)

    if (length(v) > 1)

      v <- paste0(VERB, BDX, VALUE, "[", seq_along(v), "]=", v)

    if (length(v) == 1)

      v <- paste0(VERB, BDX, VALUE, "=", v)

    c(p, o, v)

  }) |>
    unname() |>
    imap(
      function(x, idx) {

      greplace(x, IDX, if (.type == "default") idx - 1 else idx)

    })
}

# create_query(
#   first_name = starts_with_("Andr"),
#   last_name = contains_("J"),
#   state = in_(c("CA", "GA", "NY"), negate = TRUE),
#   country = in_(c("CA", "GA", "NY")),
#   state_owner = c("GA", "MD"),
#   npi = npi_ex$k,
#   ccn = "01256",
#   pac = NULL,
#   .type = "medicare"
# )
#' @autoglobal
#' @noRd
create_query <- function(..., .type) {

  args <- dots_list(..., .homonyms = "error")

  .c(VERB, FIELD, OPERATOR, VALUE, IDX, BDX) %=% query_keywords(type = .type)

  discard(args, is.null) |>
    convert_named_rhs() |>
    imap(
      function(x, name) {

        p <- paste0(VERB, BDX, FIELD, name)
        o <- paste0(VERB, BDX, OPERATOR, if (is_modifier(x)) x[["operator"]] else "=")
        v <- if (is_modifier(x)) x[["value"]] else unlist(x, use.names = FALSE)

        if (length(v) > 1)

          v <- paste0(VERB, BDX, VALUE, "[", seq_along(v), "]=", v)

        if (length(v) == 1)

          v <- paste0(VERB, BDX, VALUE, "=", v)

        c(p, o, v)

      }) |>
    unname() |>
    imap(
      function(x, idx) {

        greplace(x, IDX, if (.type == "default") idx - 1 else idx)

      })
}

# new_query(
#   first_name = starts_with_("Andr"),
#   last_name = contains_("J"),
#   state = in_(c("CA", "GA", "NY"), negate = TRUE),
#   country = in_(c("CA", "GA", "NY")),
#   state_owner = c("GA", "MD"),
#   npi = npi_ex$k,
#   ccn = "01256",
#   pac = NULL,
#   .type = "medicare"
# )
new_query <- function(..., .type) {

  # args <- dots_list(..., .homonyms = "error")

  cat(query_cli(...), sep = "\n")
  cat("\n")

  qry <- create_query(..., .type = .type)

  map(qry, function(x) {
    strsplit(x, "&") |>
      unlist() |>
      append("\n")
  }) |>
    unlist() |>
    cat(sep = "\n")

  invisible(qry)

}
