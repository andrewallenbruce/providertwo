#' @noRd
#' @autoglobal
class_query <- new_class(
  name       = "class_query",
  package    = NULL,
  properties = list(
    input    = class_list,
    output   = class_list,
    string   = new_property(
      class_character,
      getter = function(self) {
        flatten_query(self@output)
      }
    )
  )
)

#' @noRd
#' @autoglobal
class_modifier <- new_class(
  name       = "class_modifier",
  abstract   = TRUE,
  package    = NULL,
  properties = list(
    operator = new_property(class_character, default = "="),
    value    = class_character | class_numeric | NULL,
    allowed  = class_character))
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

#' @examplesIf interactive()
#' equals(1000)
#' equals(1000, negate = TRUE)
#' @autoglobal
#' @noRd
equals <- new_class(
  name        = "equals",
  package     = NULL,
  parent      = class_modifier,
  constructor = function(x, negate = FALSE) {

    check_bool(negate)

    new_object(
      S7_object(),
      operator = ifelse(!negate, "=", "<>"),
      value    = x,
      allowed  = c("caid", "prov", "open", "hgov", "care"))
  }
)

#' @examplesIf interactive()
#' greater_than(1000)
#' greater_than(0.125, or_equal = TRUE)
#' @autoglobal
#' @noRd
greater_than <- new_class(
  name        = "greater_than",
  package     = NULL,
  parent      = class_modifier,
  constructor = function(x, or_equal = FALSE) {

    check_number_decimal(x)
    check_bool(or_equal)

    new_object(
      S7_object(),
      operator = ifelse(!or_equal, ">", ">="),
      value    = x,
      allowed  = c("caid", "prov", "open", "hgov", "care"))
  }
)

#' @examplesIf interactive()
#' less_than(1000)
#' less_than(0.125, or_equal = TRUE)
#' @autoglobal
#' @noRd
less_than <- new_class(
  name        = "less_than",
  package     = NULL,
  parent      = class_modifier,
  constructor = function(x, or_equal = FALSE) {

    check_number_decimal(x)
    check_bool(or_equal)

    new_object(
      S7_object(),
      operator = ifelse(!or_equal, "<", "<="),
      value    = x,
      allowed  = c("caid", "prov", "open", "hgov", "care"))
  }
)


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
