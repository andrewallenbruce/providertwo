#' @autoglobal
#' @noRd
are_full_formulas <- function(x) {
  map_lgl(x, is_formula, lhs = TRUE)
}

#' @autoglobal
#' @noRd
are_rhs_formulas <- function(x) {
  map_lgl(x, is_formula, lhs = FALSE)
}

#' @autoglobal
#' @noRd
are_unnamed_formulas <- function(x) {
  !have_name(x) & are_full_formulas(x)
}

#' @autoglobal
#' @noRd
are_named_rhs <- function(x) {
  have_name(x) & are_rhs_formulas(x)
}

#' @autoglobal
#' @noRd
query_cli <- function(x) {

  if (any(are_named_rhs(x))) {

    x[are_named_rhs(x)] <- map(
      x[are_named_rhs(x)], function(x) {
        cli::col_green(deparse1(f_rhs(x)))
      })

  }

  if (any(are_length_two(x))) {

    x[are_length_two(x)] <- brackets(
      cli::col_yellow(
        unlist(
          x[are_length_two(x)],
          use.names = FALSE)
      )
    )
  }

  if (any(are_null(x))) x[are_null(x)] <- cli::col_red("NULL")

  if (any(are_length_one_not_null(x))) {

    x[are_length_one_not_null(x)] <- cli::col_yellow(
      unlist(
        x[are_length_one_not_null(x)],
        use.names = FALSE)
    )
  }

  VALUE <- fmt_left(unlist(x, use.names = FALSE))

  FIELD <- fmt_right(names(x))

  EQUAL <- cli::col_black("=")

  glue("{FIELD} {EQUAL} {VALUE}")
}

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

# x <- list(
#   first ~ starts_with_("Andr"),
#   first = ~starts_with_("Andr"),
#   first = "Andrew")
# # MISSES the unnamed formula
# check_names_unique(x)
# THIS IS UNNEEDED IF USING DOTS
#' @autoglobal
#' @noRd
check_names_unique <- function(x, call = caller_env()) {

  if (anyDuplicated(names(x[have_name(x)]))) {

    x <- names(x[have_name(x)])
    i <- which_(fduplicated(x))

    cli_abort(
      c("x" = "Field{?s} {.field {x[i]}} appea{?rs/rs/r} multiple times."),
      # TODO add more info ala check_unnamed_formula
      call = call)
  }
}

# x <- list(first ~ starts_with_("Andr"))
# check_unnamed_formula(x)
# THIS IS UNNEEDED IF USING DOTS
#' @autoglobal
#' @noRd
check_unnamed_formulas <- function(x, call = caller_env()) {

  if (any(are_unnamed_formulas(x))) {
    x <- map(
      x[are_unnamed_formulas(x)],
      function(x)
        as_label(x)
    ) |>
      unlist(use.names = FALSE) |>
      set_names("*")

    cli_abort(
      c("x" = "{.emph {length(x)} Unnamed formula{?s}} detected:",
        cli::col_yellow(cli::format_bullets_raw(x))),
      call = call)
  }
}

# x <- list(state = ~ in_(c("CA", "GA", "NY")))
# convert_named_formula(x)
# THIS IS UNNEEDED IF USING DOTS
#' @autoglobal
#' @noRd
convert_named_rhs <- function(x) {

  if (any(are_named_rhs(x))) {

    x[are_named_rhs(x)] <- map(
      x[are_named_rhs(x)],
      function(x)
        f_rhs(x) |>
        eval_bare()
    )
  }
  x
}

# x <- list(last_name ~ contains_("J"))
# convert_unnamed_formula(x)
# THIS IS UNNEEDED IF USING DOTS
#' @autoglobal
#' @noRd
convert_unnamed_formula <- function(x) {

  if (any(are_unnamed_formulas(x))) {

    i   <- are_unnamed_formulas(x)
    tmp <- x[i]
    rhs <- map(tmp, f_rhs)
    lhs <- map(tmp, function(x)
      f_lhs(x) |> as_string()) |>
      list_c()

    x[i] <- rhs

    names(x)[i] <- lhs
  }
  x
}

#' @autoglobal
#' @rdname query-modifiers
#' @noRd
greater_than_ <- function(x, or_equal = FALSE) {

  check_number_decimal(x)
  check_bool(or_equal)

  modifier_(
    operator = ifelse(!or_equal, ">", ">="),
    value    = x)
}

#' @autoglobal
#' @rdname query-modifiers
#' @noRd
less_than_ <- function(x, or_equal = FALSE) {

  check_number_decimal(x)
  check_bool(or_equal)

  modifier_(
    operator = ifelse(!or_equal, "<", "<="),
    value    = x)
}

#' @autoglobal
#' @rdname query-modifiers
#' @noRd
in_ <- function(x, negate = FALSE) {

  check_bool(negate)

  modifier_(
    operator = ifelse(!negate, "IN", "NOT IN"),
    value    = x)
}

# x <- list(
#   state = ~ in_(c("CA", "GA", "NY")),
#   last_name = ~ contains_("J")
#   )
# x
# convert_modifier(x)
#' @autoglobal
#' @noRd
convert_modifier <- function(x) {

  x <- convert_named_rhs(x)

  if (any(is_modifier(x))) {

    i  <- x[is_modifier(x)]

    list(
      name = names(i),
      operator = get_elem(i, "O") |> unlist(use.names = FALSE),
      value = get_elem(i, "V") |> unlist(use.names = FALSE)
    )

    # map(l, function(x) {
    #   list2(
    #     "{x['name']}" := x['value'],
    #     operator = x['operator']
    #   )
    #
    # })

    # list2(
    #   "{name}" := value,
    #   operator = operator
    # )

  }
  x
}

# create_query(
#   first_name = starts_with_("Andr"),
#   last_name = contains_("J"),
#   state = in_(c("CA", "GA", "NY")),
#   country = not_in_(c("CA", "GA", "NY")),
#   owner = c("GA", "MD"),
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

q2 <- list(
  `conditions[0][property]` = "state",
  `conditions[0][operator]` = "IN",
  `conditions[0][value][1]` = "CA",
  `conditions[0][value][2]` = "GA",
  `conditions[0][value][3]` = "NY")

# encodeString(x, quote = '"')

x <- list(
  first_name  ~ starts_with_("Andr"),
  last_name   ~ contains_("J"),
  state       = ~ in_(c("CA", "GA", "NY")),
  country     = in_(c("CA", "GA", "NY")),
  state_owner = c("GA", "MD"),
  npi         = npi_ex$k,
  npi_owner   = npi_ex$k[1],
  ccn         = "01256",
  pac         = NULL
)

x
x[map_lgl(x, is_modifier)]

rlang::is_function(contains_)

is_unnamed_formula(x)

x[is_named_rhs(x)]
!is_unnamed_formula(x) & !is_named_formula(x) & is_length_one(x)
!is_unnamed_formula(x) & !is_named_formula(x) & is_length_two(x)

check_names_unique(x)
check_unnamed_formulas(x)
convert_named_formula(x)

x = list(
  state = ~ in_(c("GA", "MD"), negate = TRUE),
  state2 = ~ in_(c("GA", "MD")),
  last_name = "SMITH",
  npi = 1234567890,
  PECOS = NULL
)

generate_query(x)
query_cli(x)
query_formatter(x)

`if`(TRUE, 1234567890, c("GA", "MD"))

# in_(c("GA", "MD"), negate = TRUE)
nf <- x[is_named_rhs(x)]
map(nf, \(x) call_args(x))


rlang::call_args_names(nf$state)

rlang::call_args(nf$state)[!have_name(rlang::call_args(nf$state))]
rlang::call_args(nf$state)[have_name(rlang::call_args(nf$state))]

rlang::call_name(nf$state)
enexpr(c("GA", "MD"))
