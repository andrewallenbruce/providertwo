# query(
#   first_name = starts_with_("Andr"),
#   last_name = contains_("J"),
#   state = in_(c("CA", "GA", "NY")),
#   country = in_(c("CA", "GA", "NY")),
#   state_owner = c("GA", "MD"),
#   npi = npi_ex$k,
#   ccn = "01256",
#   pac = NULL,
#   .type = "default"
# )
#
# query(
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


#' Create a Query Object
#'
#' @param ... Query arguments. See details for valid query modifiers.
#' @param .type Query type, `"default"` or `"medicare"`.
#' @returns S7 `<class_query>` object.
#' @examples
#' query(
#'   first_name = starts_with_("Andr"),
#'   last_name = contains_("J"),
#'   state = in_(c("CA", "GA", "NY")),
#'   city = equals_(c("Atlanta", "Los Angeles"), negate = TRUE),
#'   state_owner = c("GA", "MD"),
#'   npi = npi_ex$k,
#'   ccn = "01256",
#'   pac = NULL,
#'   .type = "medicare")
#'
#' query(
#'   first_name = starts_with_("Andr"),
#'   last_name = contains_("J"),
#'   state = in_(c("CA", "GA", "NY")),
#'   city = equals_(c("Atlanta", "Los Angeles"), negate = TRUE),
#'   state_own = c("GA", "MD"),
#'   npi = npi_ex$k,
#'   ccn = "01256",
#'   pac = NULL,
#'   .type = "default")
#' @autoglobal
#' @export
query <- function(..., .type = c("default", "medicare")) {

  .type <- arg_match(.type)

  args <- discard(dots_list(..., .homonyms = "error"), is.null)

  .c(VRB, FLD, OPR, VAL, IDX, BDX) %=% query_keywords(type = .type)

  out <- imap(args, function(x, name) {

    fields    <- paste0(VRB, BDX, FLD, name)
    operators <- paste0(VRB, BDX, OPR, if (is_modifier(x)) x[["operator"]] else "=")
    values    <- if (is_modifier(x)) x[["value"]] else unlist(x, use.names = FALSE)

    if (length(values) > 1)

      values <- paste0(VRB, BDX, VAL, "[", seq_along(values), "]=", values)

    if (length(values) == 1)

      values <- paste0(VRB, BDX, VAL, "=", values)

    c(fields, operators, values)

  }) |>
    unname() |>
    imap(function(x, i) greplace(x, IDX, ifelse(.type == "default", i - 1, i)))

  class_query(
    input  = enexprs(...),
    output = out)
}

# query <- new_class(
#   name = "query",
#   package = NULL,
#   properties = list(
#     input  = class_list,
#     output = class_list,
#     string = new_property(
#       class_character,
#       getter = function(self) {
#         flatten_query(self@output)
#       }
#     )
#   ),
#   constructor = function(..., .type) {
#     new_object(
#       S7_object(),
#       input  = enexprs(...),
#       output = process_query(..., .type = .type))
#   }
# )

