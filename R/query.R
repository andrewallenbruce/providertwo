#' @autoglobal
#' @noRd
flatten_query <- function(x) {
  map(x, function(x) paste0(x, collapse = "&")) |>
    unlist(use.names = FALSE) |>
    paste0(collapse = "&")
}

#' @noRd
#' @autoglobal
class_query <- new_class(
  name       = "class_query",
  package    = NULL,
  properties = list(
    input    = class_list,
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
#' @examples
#' query(
#'   first_name = starts_with("Andr"),
#'   last_name  = contains("J"),
#'   state      = any_of(c("CA", "GA", "NY")),
#'   city       = equals(c("Atlanta", "Los Angeles"), negate = TRUE),
#'   state_own  = c("GA", "MD"),
#'   npi        = npi_ex$k,
#'   ccn        = "01256",
#'   rate       = between(0.45, 0.67))
#' @autoglobal
#' @export
query <- function(...) {

  a <- discard(dots_list(..., .homonyms = "error"), is.null)
  i <- discard(enexprs(...), is.null)

  if ("year" %in% names(a)) {

    idx <- which_(names(a) != "year")
    a   <- a[idx]
    n   <- paste0(just_right(names(i)[idx]), " = ", i[idx])

  } else {
    n   <- paste0(just_right(names(i)), " = ", i)
  }

  medicare <- query_care(a)
  default  <- query_default(a)

  class_query(
    input = i,
    format = list(
      medicare = set_names(medicare, n),
      default  = set_names(default, n)))
}

#' @autoglobal
#' @noRd
query_care <- function(args) {
  imap(args, function(x, name) {
    v <- `if`(is_modifier(x), x@value, unlist(x, use.names = FALSE))

    c(
      paste0("filter[<<i>>][condition][path]=", name),
      paste0(
        "filter[<<i>>][condition][operator]=",
        `if`(is_modifier(x), toupper(x@operator), "=")
      ),
      `if`(
        length(v) > 1,
        paste0("filter[<<i>>][condition][value][", seq_along(v), "]=", v),
        paste0("filter[<<i>>][condition][value]=", v)
      )
    )
  }) |>
    unname() |>
    imap(function(x, idx)
      greplace(x, "<<i>>", idx))
}

#' @autoglobal
#' @noRd
query_default <- function(args) {
  imap(args, function(x, name) {
    v <- `if`(is_modifier(x), x@value, unlist(x, use.names = FALSE))

    c(
      paste0("conditions[<<i>>][property]=", name),
      paste0(
        "conditions[<<i>>][operator]=",
        `if`(is_modifier(x), tolower(x@operator), "=")
      ),
      `if`(
        length(v) > 1,
        paste0("conditions[<<i>>][value][", seq_along(v), "]=", v),
        paste0("conditions[<<i>>][value]=", v)
      )
    )
  }) |>
    unname() |>
    imap(function(x, idx)
      greplace(x, "<<i>>", idx - 1))
}
