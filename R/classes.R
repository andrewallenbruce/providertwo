#' @noRd
#' @autoglobal
class_dimensions <- new_class(
  name       = "class_dimensions",
  package    = NULL,
  properties = list(
    limit = class_integer,
    rows  = class_integer,
    pages = new_property(
      class_integer,
      getter = function(self)
        offset_size(self@rows, self@limit)
    ),
    fields = class_list
  )
)

#' @noRd
#' @autoglobal
class_current <- new_class(
  name       = "class_current",
  package    = NULL,
  parent     = class_character
)

#' @noRd
#' @autoglobal
class_temporal <- new_class(
  name       = "class_temporal",
  package    = NULL,
  parent     = class_list
)

#' @noRd
#' @autoglobal
class_endpoint <- new_class(
  name         = "class_endpoint",
  package      = NULL,
  abstract     = TRUE,
  properties   = list(
    metadata   = class_list,
    dimensions = class_dimensions,
    identifier = class_current | class_temporal
  )
)

#' @noRd
#' @autoglobal
class_care <- new_class(
  name = "class_care",
  package = NULL,
  parent = class_endpoint,
  properties = list(
    resources = class_current | class_temporal
  )
)

#' @noRd
#' @autoglobal
class_prov <- new_class(
  name = "class_prov",
  package = NULL,
  parent = class_endpoint,
  properties = list(
    identifier = class_current
  )
)

#' @noRd
#' @autoglobal
class_caid <- new_class(
  name = "class_caid",
  package = NULL,
  parent = class_endpoint
)

#' @noRd
#' @autoglobal
class_open <- new_class(
  name = "class_open",
  package = NULL,
  parent = class_endpoint
)

#' @noRd
#' @autoglobal
class_hgov <- new_class(
  name = "class_hgov",
  package = NULL,
  parent = class_endpoint
)

#' @noRd
#' @autoglobal
class_group <- new_class(
  name       = "class_group",
  package    = NULL,
  properties = list(
    name    = class_character,
    members = class_list
  )
)

#' @noRd
#' @autoglobal
class_collection <- new_class(
  name       = "class_collection",
  package    = NULL,
  parent     = class_group
)

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
