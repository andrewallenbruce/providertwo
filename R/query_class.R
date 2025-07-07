# process_query(
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
#' @noRd
#' @autoglobal
process_query <- function(..., .type) {

  if (is_missing(.type)) .type <- "default"

  args <- discard(dots_list(..., .homonyms = "error"), is.null)

  .c(VRB, FLD, OPR, VAL, IDX, BDX) %=% query_keywords(type = .type)

  imap(args, function(x, name) {

    fields    <- paste0(VRB, BDX, FLD, name)
    operators <- paste0(VRB, BDX, OPR, ifelse(is_modifier(x), x[["operator"]], "="))
    values    <- ifelse(is_modifier(x), x[["value"]], unlist(x, use.names = FALSE))
    values    <- paste0(VRB, BDX, VAL, brack_along(values), "=", values)

    c(fields, operators, values)

  }) |>
    unname() |>
    imap(function(x, i) greplace(x, IDX, ifelse(.type == "default", i - 1, i)))
}

# class_query(
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
#' @noRd
#' @autoglobal
class_query <- new_class(
  name = "class_query",
  package = NULL,
  properties = list(
    input = class_list,
    raw   = class_list,
    valid = new_property(
      class_character,
      getter = function(self) {
        flatten_query(self@raw)
      }
    )
  ),
  constructor = function(..., .type = "default") {
    new_object(S7_object(),
               input = enexprs(...),
               raw   = process_query(..., .type))
  }
)
