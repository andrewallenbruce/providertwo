query2(
  year = 2022:2024,
  state = c("GA", "NY"),
  enrlmt_id = "I20040309000221",
  city = matches("Atlanta"),
  provcity = "Atlanta",
  provider_name = starts_with("C"),
  provname = starts_with("C"),
  provider_first_name = starts_with("An"),
  provider_last_name = contains("JE"),
  practice_state_or_us_territory = c("GA", "FL"),
  practice_size = less_than(10, or_equal = TRUE),
  or("state", "city"),
  or("provider_name", "provider_first_name"))

query2(
  npi = 1003879883,
  pecos_asct_cntl_id = 3678485208,
  enrlmt_id = "I20040309000221",
  provider_type_cd = c("14-16", "14-39"),
  provider_type_desc = ends_with("OLOGY"),
  state_cd = c("GA", "NY"),
  first_name = starts_with("An"),
  last_name = contains("Z"),
  org_name = contains("E"),
  or("state_cd", "org_name"),
  or("npi", "pecos_asct_cntl_id", "enrlmt_id")
)

kronecker(
  X = c("state", "city", "A"),
  Y = c("provider_name", "provider_first_name"),
  FUN = paste0
)

# fs::file_create(fs::path_package("providertwo"), "test.toml")
# tomledit::as_toml(end_open)
# tomledit::write_toml(fs::path_package("providertwo"))
# tomledit::read_toml(fs::path_package("providertwo", "test.toml"))

# TODO Explore chunking params with 10+ elements
# Submitting too many params at once can cause API errors
# TODO Consider the case sensitivity of each endpoint's fields' values
# Some are all uppercase and won't match anything but uppercase values

#' @noRd
#' @autoglobal
query2 <- function(...) {
  x <- list(
    groups = purrr::keep(rlang::enexprs(...), \(x) is_junc(x)),
    params = purrr::discard(rlang::enexprs(...), \(x) is_junc(x))
  )

  g_names <- purrr::map(x$groups, function(x)
    S7::prop(eval(x), "members")) |>
    unlist(use.names = FALSE) |>
    collapse::funique()

  ok <- all(g_names %in_% rlang::names2(x$params))

  if (!ok) {
    cli::cli_abort(
      c("x" = "All {.field group} members must be {.field query} field names"),
      call = caller_env())
  }

  class_query(
    params = purrr::map(x$params, eval),
    groups = rlang::set_names(
      purrr::map(x$groups, eval),
      paste0("g", seq_along(x$groups)))
  )
}

ex_class_query <- S7::new_class(
  name       = "class_query",
  package    = NULL,
  properties = list(
    input    = S7::class_list,
    params   = S7::class_list,
    groups   = S7::new_property(
      S7::class_list,
      validator = function(self, value) {
        if (!all(c("members") %in% names2(value))) {
          stop("`groups` must contain `members`.", call. = FALSE)
        }
        if (!is.character(value$members)) {
          stop("`groups$members` must be a character vector.", call. = FALSE)
        }
        TRUE
      }
    ),
    named    = S7::new_property(
      S7::class_list,
      getter = function(self) {
        identical(names2(self@params), self@groups$members)
      }
    )
  )
)

ex_class_query(
  params = list(a = 10, b = 20),
  groups = list(
    members = c("a", "bc")
  )
)

q3 <- function(...) {
  list(
    groups = purrr::keep(rlang::enexprs(...), \(x) is_junc(x)),
    params = purrr::discard(rlang::enexprs(...), \(x) is_junc(x))
  )
}

x <- q3(
  first_name  = starts_with("And"),
  middle_name = NULL,
  last_name   = contains("J"),
  state       = any_of(c("CA", "GA", "NY")),
  state_own   = c("GA", "MD"),
  npi         = npi_ex$k,
  ccn         = "01256",
  rate        = between(0.45, 0.67),
  year        = 2014:2025,
  or("first_name", "last_name"),
  and("ccn", "npii"))

g_names <- purrr::map(x$groups, function(x)
  prop(eval(x), "members")) |>
  unlist(use.names = FALSE) |>
  funique()

ok <- all(g_names %in_% names2(x$params))

if (!ok) {
  cli::cli_abort(
    c("x" = "All {.field group} members must be {.field query} field names"),
    call = caller_env())
}

query2(
  first_name  = starts_with("And"),
  middle_name = NULL,
  last_name   = contains("J"),
  state       = any_of("CA", "GA", "NY"),
  state_own   = c("GA", "MD"),
  npi         = npi_ex$k,
  ccn         = "01256",
  rate        = between(0.45, 0.67),
  year        = 2014:2025,
  or("first_name", "last_name"),
  or("state", "state1")
)

str_dist <- function(x, y) {
  stringdist::stringdist(x, y, method = "jaccard")
}

purrr::imap(
  unique_fields, \(x, i) {
    setequal(x, y = unique_fields$`2017`)
  }
)

unique_fields$`2021`

stringdist::stringdist(
  unique_fields$`2017`,
  unique_fields$`2018`,
  method = "jaccard")

intersect(unique_fields$`2023`, unique_fields$`2022`)


set_len <- collapse::vlengths(unique_fields)

field_groups <- set_names(
  vctrs::vec_split(unique_fields, set_len)$val,
  paste0("ln_", collapse::funique(set_len)))

setequal(unique_fields$`2023`, unique_fields$`2022`)


RapidFuzz::extract_similar_strings(
  unique_fields$`2023`[1],
  unique_fields$`2022`,
  score_cutoff = 99)

#' @autoglobal
#' @noRd
map_parse_eval <- function(x) {
  purrr::map(x, function(x) {
    rlang::parse_expr(x) |>
      rlang::eval_bare()
  })
}

#' @autoglobal
#' @noRd
as_glue_list <- function(x) {
  purrr::map(x, function(x) {
    glue::as_glue("list(") +
      glue::glue_collapse(x, ", ") +
      glue::as_glue(")")
  })
}

#' @autoglobal
#' @noRd
finalize_match2 <- function(x) {
  combo_cd <- paste0(
    "{glue::backtick(field)} = ",
    "{ifelse(ismod, unlist(param, use.names = FALSE), ",
    "glue::double_quote(unlist(param, use.names = FALSE)))}"
  )

  x <- collapse::mtt(x,
                     ismod = purrr::map_lgl(param, is_mod),
                     combo = glue::glue(combo_cd),
                     ismod = NULL,
                     field = NULL,
                     param = NULL) |>
    collapse::rsplit(~ year) |>
    cheapr::cheapr_rev() |> # rsplit reverses order of years
    as_glue_list()

  map_parse_eval(x)
}

class_junction <- S7::new_class(
  name       = "class_junction",
  package    = NULL,
  properties = list(
    conjunction = S7::class_character,
    members     = S7::class_character) #,
  # validator = function(self) {
  #   if (length(self@conjunction) != 1L) {
  #     cli::cli_abort(c("x" = "{.field @conjunction} must be length 1"))
  #   }
  #   if (!self@conjunction %in% c("OR", "AND")) {
  #     cli::cli_abort(c("x" = "{.field @conjunction} must be one of {.val AND} or {.val OR}"))
  #   }
  #   if (length(self@members) == 1L) {
  #     cli::cli_abort(c("x" = "{.field @members} must be greater than length 1"))
  #   }
  # }
)

#' @autoglobal
#' @noRd
query3 <- function(...) {
  class_query(
    input  = rlang::enquos(
      ...,
      .homonyms = "error",
      .named = TRUE,
      .ignore_null = "all",
      .check_assign = TRUE
    ),
    params = purrr::compact(
      rlang::dots_list(
        ...,
        .homonyms = "error",
        .named = TRUE,
        .check_assign = TRUE
      )
    )
  )
}

#' @autoglobal
#' @noRd
match_query2 <- function(obj, qry) {

  param <- not_year(qry)

  k <- keys(obj) |>
    unlist(use.names = FALSE) |>
    collapse::funique()

  vec <- set_names(k, clean_names(k))

  raw <- vec[names2(vec) %in_% names2(param)]

  values <- cheapr::cheapr_rep_each(
    param,
    cheapr::counts(
      names(raw))$count)

  named_args <- set_names(values, unname(raw))

  list(
    value = named_args,
    year = map(keys(obj), \(x) x[c_match(unname(raw), x)]))
}

#' @autoglobal
#' @noRd
map_match_query <- function(obj, qry) {

  param  <- not_year(qry)
  pname  <- rlang::names2(param)

  # TODO Use a modifier on "year"
  # parameter if meant for an API field?
  field <- keys(obj)
  clean <- purrr::map(field, clean_names)

  fin <- purrr::pmap(
    list(clean, field),
    function(cl, fl) {
      rlang::set_names(cl, fl)[c_match(pname, rlang::set_names(cl, fl))]
    }) |>
    purrr::compact()

  imap(fin, \(x, i) {
    param[names(param) %in% unlist(x, use.names = FALSE)]
  }) |>
    purrr::map2(fin, \(x, y) set_names(x, names2(y)))
}

#' @noRd
#' @autoglobal
class_query2 <- new_class(
  name       = "class_query",
  package    = NULL,
  properties = list(
    input    = new_property(class_list,
                            setter = function(self, value) {
                              self@input <- value
                              self
                            }
    ),
    format   = class_list,
    string   = new_property(
      class_character,
      getter = function(self) {
        map(self@format, flatten_query)
      }
    )
  )
)

#' Create a Query Object
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Named conditions where the names are API fields.
#'
#' @returns S7 `<class_query>` object.
#'
#' @examplesIf interactive()
#' query(
#'   first_name = starts_with("Andr"),
#'   last_name  = contains("J"),
#'   state      = any_of(c("CA", "GA", "NY")),
#'   city       = equals(c("Atlanta", "Los Angeles"), negate = TRUE),
#'   state_own  = c("GA", "MD"),
#'   npi        = npi_ex$k,
#'   ccn        = "01256",
#'   rate       = between(0.45, 0.67),
#'   year       = 2014:2025)
#' @autoglobal
#' @noRd
query <- function(...) {
  params <- compact(dots_list(..., .homonyms = "error"))
  inputs <- compact(enexprs(...))

  if ("year" %in% names(params)) {

    idx     <- which_(names(params) != "year")
    params  <- params[idx]
    par_nms <- paste0(just_right(names(inputs)[idx]), " = ", inputs[idx])

    idx     <- which_(names(inputs) == "year")
    yank(inputs[idx]) <- eval(yank(inputs[idx]))

  } else {
    par_nms <- paste0(just_right(names(inputs)), " = ", inputs)
  }

  class_query2(
    input  = inputs,
    format = list(
      medicare = set_names(query_care(params), par_nms),
      default  = set_names(query_default(params), par_nms)
    )
  )
}

#' @rdname query_modifier
#' @examples
#' between(1000, 1100)
#' between(0.125, 2)
#' try(between(0.95, 0.67))
#' @autoglobal
#' @export
between <- S7::new_class(
  name        = "between",
  package     = NULL,
  parent      = class_modifier,
  constructor = function(x, y) {

    check_number_decimal(x)
    check_number_decimal(y)

    if (x >= y) {
      cli::cli_abort(
        "{.field x} [{.val {x}}] must be less than {.field y} [{.val {y}}]",
        call. = FALSE)
    }

    S7::new_object(
      class_modifier(),
      operator = "BETWEEN",
      value    = c(x, y))
  }
)

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

# new_query(
#   first_name = starts_with_("Andr"),
#   last_name = contains_("J"),
#   state = in_(c("CA", "GA", "NY")),
#   country = in_(c("CA", "GA", "NY")),
#   state_owner = c("GA", "MD"),
#   npi = npi_ex$k,
#   ccn = "01256",
#   pac = NULL,
#   .type = "medicare"
# )
#' @autoglobal
#' @noRd
new_query <- function(..., .type) {

  if (is_missing(.type)) .type <- "default"

  cli::cli_h3(cli::col_yellow("Parameter Inputs:"))
  g <- params_cli(...)

  q <- process_query(..., .type = .type)

  cli::cli_h3(cli::col_yellow("Query Output:"))
  # map(q, function(x) unlist(x, use.names = FALSE) |> append("\n")) |>
  #   unlist(use.names = FALSE) |>
  #   cli::col_silver() |>
  #   cat(sep = "\n")
  strwrap(
    strsplit(flatten_query(q), "&") |>
      unlist(),
    width = 70,
    prefix = paste0(cli::symbol$full_block, cli::symbol$upper_block_1, " ")
  ) |> cli::col_silver() |> cat(sep = "\n")

  invisible(flatten_query(q))

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

#' Query Modifiers
#'
#' @description
#' Helpers for use in constructing conditions in queries.
#'
#' @details
#' Query modifiers are a small DSL for use in constructing query conditions,
#' in the [JSON-API](https://www.drupal.org/docs/core-modules-and-themes/core-modules/jsonapi-module/filtering) format.
#'
#' @param x,y input
#' @param or_equal `<lgl>` append `=`
#' @param negate `<lgl>` prepend `NOT`
#' @param care `<lgl>` use uppercase operators for `care` endpoint
#' @name query_modifier
#' @returns An object of class `<modifier>`
NULL

#' @rdname query_modifier
#' @examples
#' greater_than_(1000)
#' greater_than_(0.125, or_equal = TRUE)
#' @autoglobal
#' @export
greater_than_ <- function(x, or_equal = FALSE) {

  check_number_decimal(x)
  check_bool(or_equal)

  modifier_(
    operator = ifelse(!or_equal, ">", ">="),
    value    = x,
    allow    = c("caid", "prov", "open", "hgov", "care"))
}

#' @rdname query_modifier
#' @examples
#' less_than_(1000)
#' less_than_(0.125, or_equal = TRUE)
#' @autoglobal
#' @export
less_than_ <- function(x, or_equal = FALSE) {

  check_number_decimal(x)
  check_bool(or_equal)

  modifier_(
    operator = ifelse(!or_equal, "<", "<="),
    value    = x,
    allow    = c("caid", "prov", "open", "hgov", "care"))
}

#' @rdname query_modifier
#' @examples
#' between_(1000, 1100)
#' between_(0.125, 2, negate = TRUE) # should ignore `negate`
#' between_(0.125, 2, care = TRUE, negate = TRUE)
#' @autoglobal
#' @export
between_ <- function(x, y, care = FALSE, negate = FALSE) {

  check_number_decimal(x)
  check_number_decimal(y)

  if (x >= y) cli::cli_abort("`x` must be less than `y`.", call. = FALSE)

  check_bool(care)
  check_bool(negate)

  modifier_(
    operator = if (!care) "between" else ifelse(!negate, "BETWEEN", "NOT+BETWEEN"),
    value    = c(x, y),
    allow    = if (!care) c("caid", "prov", "open", "hgov") else "care")
}

#' @rdname query_modifier
#' @examples
#' starts_with_("foo")
#' starts_with_("foo", care = TRUE)
#' @autoglobal
#' @export
starts_with_ <- function(x, care = FALSE) {

  check_bool(care)

  modifier_(
    operator = if (!care) "starts+with" else "STARTS_WITH",
    value    = x,
    allow    = if (!care) c("caid", "prov", "open", "hgov") else "care")
}

#' @rdname query_modifier
#' @examples
#' ends_with_("bar")
#' @autoglobal
#' @export
ends_with_ <- function(x) {

  check_character(x, allow_na = FALSE)

  modifier_(
    operator = "ENDS_WITH",
    value    = x,
    allow    = "care")

}

#' @rdname query_modifier
#' @examples
#' contains_("baz")
#' contains_("baz", care = TRUE)
#' @autoglobal
#' @export
contains_ <- function(x, care = FALSE) {

  check_bool(care)

  modifier_(
    operator = if (!care) "contains" else "CONTAINS",
    value    = x,
    allow    = if (!care) c("caid", "prov", "open", "hgov") else "care")
}

#' @rdname query_modifier
#' @examples
#' like_("baz")
#' @autoglobal
#' @export
like_ <- function(x) {

  modifier_(
    operator = "like",
    value    = x,
    allow    = c("caid", "prov", "open", "hgov"))
}

#' @rdname query_modifier
#' @examples
#' in_(state.abb[10:15])
#' in_(state.abb[10:15], negate = TRUE)
#' in_(state.abb[1:5], care = TRUE)
#' in_(state.abb[1:5], care = TRUE, negate = TRUE)
#' @autoglobal
#' @export
in_ <- function(x, care = FALSE, negate = FALSE) {

  check_bool(negate)
  check_bool(care)

  modifier_(
    operator = if (!care) ifelse(!negate, "in", "not+in") else ifelse(!negate, "IN", "NOT+IN"),
    value    = x,
    allow    = if (!care) c("caid", "prov", "open", "hgov") else "care")
}

#' @rdname query_modifier
#' @examples
#' equals_(1000)
#' equals_(1000, negate = TRUE)
#' @autoglobal
#' @export
equals_ <- function(x, negate = FALSE) {

  check_bool(negate)

  modifier_(
    operator = ifelse(!negate, "=", "<>"),
    value    = x,
    allow    = c("caid", "prov", "open", "hgov", "care"))
}

#' Query Modifier Constructor
#'
#' @param operator `<chr>` comparison operator
#' @param value `<any>` value to compare against
#' @param allow `<chr>` allowed endpoint class(es) for this modifier
#' @returns An object of class `"modifier"`
#' @examples
#' modifier_(">", 1000, "all")
#' @autoglobal
#' @keywords internal
#' @export
modifier_ <- function(operator, value, allow) {

  check_required(operator)
  check_required(allow)

  all  <- c("=", "<>", "<", "<=", ">", ">=")
  ohcp <- c("like", "between", "in", "not+in", "contains", "starts+with", "match")
  prov <- c("is_empty", "not_empty")
  care <- c("NOT+BETWEEN", "BETWEEN", "IN", "NOT+IN", "CONTAINS",
            "STARTS_WITH", "ENDS_WITH", "IS+NULL", "IS+NOT+NULL")

  arg_match0(operator, values = cheapr::cheapr_c(all, ohcp, prov, care))

  structure(
    list(
      operator = operator,
      value    = value,
      allow    = allow),
    class = "modifier")
}

#' Print method for query modifier
#' @param ... additional arguments
#' @rdname query_modifier
#' @method print modifier
#' @autoglobal
#' @noRd
print.modifier <- function(x, ...) {

  cli::cli_text(cli::col_cyan("<modifier>"))
  cli::cli_text(c(cli::col_silver("Operator: "), cli::col_red(x$operator)))

  if (!is.null(x$value)) {
    cli::cli_text(c(cli::col_silver("Value(s): "), cli::col_yellow("{x$value}")))
  }
  cli::cli_text(c(cli::col_silver("Allowed: "), brackets_cli2(sort(x$allow))))
}

#' Query modifier check
#' @param x input
#' @returns `<lgl>` TRUE or FALSE
#' @examples
#' is_modifier(greater_than_(1000))
#' @autoglobal
#' @keywords internal
#' @export
is_modifier <- function(x) {
  inherits(x, "modifier")
}

between <- function(x, y, negate = FALSE) {

  check_number_decimal(x)
  check_number_decimal(y)
  check_bool(negate)

  if (x >= y) cli::cli_abort("`x` must be less than `y`.", call. = FALSE)

  modifier_(
    operator = if (!care) "between" else ifelse(!negate, "BETWEEN", "NOT+BETWEEN"),
    value    = c(x, y),
    allow    = if (!care) c("caid", "prov", "open", "hgov") else "care")
}

#' @rdname query_modifier
#' @examples
#' equals_(1000)
#' equals_(1000, negate = TRUE)
#' @autoglobal
#' @export
equals_ <- function(x, negate = FALSE) {

  check_bool(negate)

  modifier_(
    operator = ifelse(!negate, "=", "<>"),
    value    = x,
    allow    = c("caid", "prov", "open", "hgov", "care"))
}


# operators <- function(x) {
#   open <- c(
#     "=",
#     "<>",
#     "<",
#     "<=",
#     ">", ">=",
#     "like",
#     "between",
#     "in",
#     "not+in",
#     "contains",
#     "starts+with",
#     "match"
#   )
#
#   hgov <- c(
#     "=",
#     "<>",
#     "<",
#     "<=",
#     ">",
#     ">=",
#     "like",
#     "between",
#     "in",
#     "not+in",
#     "contains",
#     "starts+with",
#     "match"
#   )
#
#   caid <- c(
#     "=",
#     "<>",
#     "<",
#     "<=",
#     ">",
#     ">=",
#     "in",
#     "not+in",
#     "contains",
#     "starts+with",
#     "between",
#     "like",
#     "match"
#   )
#
#   prov <- c(
#     "=",
#     "<>",
#     "<",
#     "<=",
#     ">",
#     ">=",
#     "in",
#     "not+in",
#     "contains",
#     "starts+with",
#     "between",
#     "is_empty",
#     "not_empty",
#     "like",
#     "match"
#   )
#
#   care <- c(
#     "=",
#     "<>",
#     "<",
#     "<=",
#     ">",
#     ">=",
#     "IN",
#     "NOT+IN",
#     "CONTAINS",
#     "STARTS_WITH",
#     "ENDS_WITH",
#     "BETWEEN",
#     "NOT+BETWEEN",
#     "IS+NULL",
#     "IS+NOT+NULL"
#   )
#
#   clog_rep <- function(x, name) {
#     cheapr::cheapr_rep_len(name, length(x))
#   }
#
#   fastplyr::new_tbl(
#     catalog = c(clog_rep(open, "open"),
#                 clog_rep(hgov, "hgov"),
#                 clog_rep(caid, "caid"),
#                 clog_rep(prov, "prov"),
#                 clog_rep(care, "care")),
#     operator = c(open, hgov, caid, prov, care) # |> tolower()
#   ) |>
#     fastplyr::f_add_count(operator, sort = TRUE) |>
#     print(n = Inf)
#
# }


# `%AND%` <- function(lhs, rhs) {
# group AND
#
# filter[g1][group][conjunction]=AND
# filter[1][condition][memberOf]=g1
# filter[2][condition][memberOf]=g1
#
# filter[1][condition][path]=first_name
# filter[1][condition][operator]==
# filter[1][condition][value]=Janis
#
# filter[2][condition][path]=last_name
# filter[2][condition][operator]=STARTS_WITH
# filter[2][condition][value]=J
# }

`%or%` <- function(lhs, rhs) {

filter[g1][group][conjunction]=OR
filter[1][condition][memberOf]=g1
filter[2][condition][memberOf]=g1

filter[1][condition][path]=PROVIDER_TYPE_DESC
filter[1][condition][operator]=CONTAINS
filter[1][condition][value]=PRACTITIONER

filter[2][condition][path]=STATE_CD
filter[2][condition][operator]==
filter[2][condition][value]=MD
}

`%or%` <- function(lhs, rhs, .env = NULL) {

  lhs <- rlang::enquo(lhs)
  rhs <- rlang::enquo(rhs)

  if (!rlang::is_null(.env)) x <- rlang::quo_set_env(x, env = .env)

  cheapr::list_combine(lhs = lhs, rhs = rhs)
}

x <- list(first_name = starts_with("And") %or% contains("J"))

typeof(x$first_name[[1]])

rlang::eval_tidy(x$first_name[[1]])

# - filter[g1][group][conjunction]=OR
# --- filter[1][condition][memberOf]=g1
# --- filter[2][condition][memberOf]=g1
#
# filter[1][condition][path]=PROVIDER_TYPE_DESC
# filter[1][condition][operator]=CONTAINS
# filter[1][condition][value]=PRACTITIONER
#
# filter[2][condition][path]=STATE_CD
# filter[2][condition][operator]==
# filter[2][condition][value]=MD



#' @rdname query_modifier
#' @examples
#' # Not working for any endpoint yet
#' match_("baz")
#' @autoglobal
#' @noRd
match_ <- function(x) {

  modifier_(
    operator = "match",
    value    = x,
    allow    = c("caid", "prov", "open", "hgov"))
}

#' @rdname query_modifier
#' @examples
#' # Not working for any endpoint yet
#' is_null_()
#' is_null_(negate = TRUE)
#' @autoglobal
#' @noRd
is_null_ <- function(negate = FALSE) {

  check_bool(negate)

  modifier_(
    operator = ifelse(!negate, "IS+NULL", "IS+NOT+NULL"),
    value    = NULL,
    allow    = "care")
}

#' @rdname query_modifier
#' @examples
#' # Not working for any endpoint yet
#' is_empty_()
#' is_empty_(negate = TRUE)
#' @autoglobal
#' @noRd
is_empty_ <- function(negate = FALSE) {

  check_bool(negate)

  modifier_(
    operator = ifelse(!negate, "is_empty", "not_empty"),
    value    = NULL,
    allow    = "prov")
}

#' @autoglobal
#' @noRd
any_modifiers <- function(x) {
  any(purrr::map_lgl(x, is_modifier), na.rm = TRUE)
}

#' @autoglobal
#' @noRd
any_junc <- function(x) {
  any(purrr::map_lgl(x, is_junc), na.rm = TRUE)
}

#' @autoglobal
#' @noRd
any_mod <- function(x) {
  any(purrr::map_lgl(x, is_mod), na.rm = TRUE)
}

#' @autoglobal
#' @noRd
deparse_mods <- function(x) {
  purrr::map(x[is_mod(x)], function(x) deparse1(x))
}

#' @autoglobal
#' @noRd
are_calls <- function(x) {
  purrr::map_lgl(x, function(x) Negate(is_mod)(x) & rlang::is_call(x))
}

#' @autoglobal
#' @noRd
any_calls <- function(x) {
  any(are_calls(x))
}

#' @autoglobal
#' @noRd
deparse_calls <- function(x) {
  purrr::map(x[are_calls(x)], function(x) deparse1(x))
}

#' @autoglobal
#' @noRd
are_evaled <- function(x) {
  cheapr::list_lengths(x) >= 1L & purrr::map_lgl(x, Negate(is_call))
}

#' @autoglobal
#' @noRd
any_evaled <- function(x) {
  any(are_evaled(x))
}

#' @autoglobal
#' @noRd
eval_cli <- function(x) {
  purrr::map(x[are_evaled(x)], function(x)
    cli::col_cyan(paste0(glue::double_quote(x), collapse = ", ")))
}

#' @autoglobal
#' @noRd
call_cli <- function(x) {
  purrr::map(x[are_calls(x)], function(x) cli::col_yellow(deparse1(x)))
}

#' @autoglobal
#' @noRd
mods_cli <- function(x) {
  purrr::map(x[is_mod(x)], function(x) cli::col_red(deparse1(x)))
}

#' @autoglobal
#' @noRd
are_length_one <- function(x) {
  cheapr::list_lengths(x) == 1L
}

#' @autoglobal
#' @noRd
are_length_two <- function(x) {
  cheapr::list_lengths(x) > 1L
}

#' @autoglobal
#' @noRd
any_length_two <- function(x) {
  any(are_length_two(x))
}

#' @autoglobal
#' @noRd
are_null <- function(x) {
  purrr::map_lgl(x, is.null)
}

#' @autoglobal
#' @noRd
any_null <- function(x) {
  any(are_null(x))
}

#' @autoglobal
#' @noRd
are_not_null <- function(x) {
  purrr::map_lgl(x, Negate(is.null))
}

# x <- new_query(
#   first_name = starts_with("Andr"),
#   last_name  = contains("J"),
#   state      = any_of(c("CA", "GA", "NY")),
#   state_own  = none_of(c("GA", "MD")),
#   npi        = npi_ex$k,
#   ccn        = "01256",
#   pac        = NULL,
#   rate       = between(0.45, 0.67),
#   year       = 2014:2025)
#
#   cli_query(x)
#' @autoglobal
#' @noRd
cli_query <- function(x) {

  x <- x@input

  if (any_evaled(x)) x[are_evaled(x)] <- eval_cli(x[are_evaled(x)])
  if (any_mod(x))    x[is_mod(x)]     <- mods_cli(x[is_mod(x)])
  if (any_calls(x))  x[are_calls(x)]  <- call_cli(x[are_calls(x)])

  FIELD  <- just_right(names(x))
  EQUALS <- cli::col_black(cli::style_bold(cli::symbol$double_line))
  VALUE  <- just_left(unlist(x, use.names = FALSE))

  cli::cli_h1("New Query:")
  cli::cat_print(glue::glue_safe("{FIELD} {EQUALS} {VALUE}"))

  invisible(x)
}

#,
# validator = function(self) {
#
#   all  <- c("=", "<>", "<", "<=", ">", ">=")
#   ohcp <- c("like", "between", "in", "not+in",
#             "contains", "starts+with", "match")
#   prov <- c("is_empty", "not_empty")
#   care <- c("NOT+BETWEEN", "BETWEEN", "IN",
#             "NOT+IN", "CONTAINS", "STARTS_WITH",
#             "ENDS_WITH", "IS+NULL", "IS+NOT+NULL")
#
#   if (self@operator %!iin% c(all, ohcp, prov, care)) "@operator is invalid"
#   if (any(self@allowed %!iin% c("caid", "care", "hgov", "open", "prov"))) "@allow is invalid"
#
# }
# )

# S7::method(build, list(class_collection, class_query)) <- function(obj, qry) {
#   S7::prop(obj, "members") |> purrr::map(\(x) build(obj = x, qry = qry))
# }

# S7::method(build, list(class_group, S7::class_missing)) <- function(obj, qry) {
#   S7::prop(obj, "members") |> purrr::map(\(x) build(obj = x))
# }
#
# S7::method(build, list(class_catalog, S7::class_missing)) <- function(obj, qry) {
#   S7::prop(obj, "access") |> build()
# }
#
# S7::method(build, list(class_current, S7::class_missing)) <- function(obj, qry) {
#   list(
#     title      = S7::prop(obj, "metadata")$title,
#     dimensions = S7::prop(obj, "dimensions"),
#     identifier = S7::prop(obj, "identifier"),
#     params     = NULL
#   )
# }
#
# S7::method(build, list(class_temporal, S7::class_missing)) <- function(obj, qry) {
#
#   id <- S7::prop(obj, "identifier")
#
#   list(
#     title      = S7::prop(obj, "metadata")$title,
#     dimensions = S7::prop(obj, "dimensions"),
#     identifier = rlang::set_names(collapse::get_elem(id, "identifier"), collapse::get_elem(id, "year")),
#     params     = NULL
#   )
# }
