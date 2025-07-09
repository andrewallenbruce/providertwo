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

#' Print method for query modifier
#' @param ... additional arguments
#' @rdname query_modifier
#' @method print modifier
#' @autoglobal
#' @export
print.modifier <- function(x, ...) {

  cli::cli_text(cli::col_cyan("<modifier>"))
  cli::cli_text(c(cli::col_silver("Operator: "), cli::col_red(x$operator)))

  if (!is.null(x$value)) {
    cli::cli_text(c(cli::col_silver("Value(s): "), cli::col_yellow("{x$value}")))
  }
  cli::cli_text(c(cli::col_silver("Allowed: "), brackets_cli2(sort(x$allow))))
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

# `%or%` <- function(lhs, rhs) {
#
# filter[g1][group][conjunction]=OR
# filter[1][condition][memberOf]=g1
# filter[2][condition][memberOf]=g1
#
# filter[1][condition][path]=PROVIDER_TYPE_DESC
# filter[1][condition][operator]=CONTAINS
# filter[1][condition][value]=PRACTITIONER
#
# filter[2][condition][path]=STATE_CD
# filter[2][condition][operator]==
# filter[2][condition][value]=MD
# }



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
